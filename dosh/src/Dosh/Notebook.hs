{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.Notebook where

import Control.Lens
import Control.Monad.Fix
import Data.Default
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Sequence qualified as Seq
import Data.Sequence.Zipper (SeqZipper (..))
import Data.Sequence.Zipper qualified as SZ
import Data.Text.CodeZipper qualified as CZ
import Data.Text.Utf16.Rope (Rope)
import Data.These (These (..))
import Data.Traversable (for)
import Data.UUID (UUID)
import Dosh.Cell
import Dosh.Cell qualified as Cell
import Dosh.GHC.Client qualified as GHC
import Dosh.LSP.Client qualified as LSP
import Dosh.Prelude
import Dosh.Util
import Language.LSP.Types (TextDocumentIdentifier (..), Uri (..))
import Language.LSP.Types qualified as LSP hiding (line)
import Language.LSP.Types.Lens qualified as LSP
import Reflex hiding (Query, Response)
import Reflex.Vty hiding (Query, Response)

data Notebook = Notebook
    { identifier :: TextDocumentIdentifier
    , cells :: HashMap UUID Cell
    , cellOrder :: SeqZipper UUID
    , nextCellNumber :: Int
    , disabled :: Bool
    , language :: Text
    , contents :: Rope
    }
    deriving stock (Generic, Show)

instance Default Notebook where
    def =
        Notebook
            { identifier = TextDocumentIdentifier $ Uri mempty
            , cells = mempty
            , cellOrder = mempty
            , nextCellNumber = 1
            , disabled = False
            , language = mempty
            , contents = mempty
            }

newNotebook :: MonadIO m => (Notebook -> Notebook) -> m Notebook
newNotebook f = do
    uri <- newRandomUri "/" ".hs"
    def
        & #identifier .~ TextDocumentIdentifier uri
        & f
        & createCell (#disabled .~ False)

-- | Create a new cell with a random UUID.
createCell :: MonadIO m => (Cell -> Cell) -> Notebook -> m Notebook
createCell f n = flip addCell n <$> newCell f

-- | Add a cell to the notebook.
addCell :: Cell -> Notebook -> Notebook
addCell c n =
    n
        & #cells . at c.uid
            ?~ ( c
                    & #number .~ n.nextCellNumber
                    & #input . #language .~ n.language
               )
        & #cellOrder %~ SZ.insertAfter c.uid

currentCellUid :: Notebook -> Maybe UUID
currentCellUid n = SZ.current n.cellOrder

overUid :: UUID -> (Cell -> Cell) -> Notebook -> Notebook
overUid uid f = #cells . ix uid %~ f

currentCell :: Notebook -> Maybe Cell
currentCell n = currentCellUid n >>= \cid -> n ^. #cells . at cid

overCurrentCell :: (Cell -> Cell) -> Notebook -> Notebook
overCurrentCell f n = maybe n ((n &) . flip overUid f) (currentCellUid n)

notebook
    :: forall t m
     . ( PerformEvent t m
       , TriggerEvent t m
       , HasInput t m
       , MonadFix m
       , HasImageWriter t m
       , HasTheme t m
       , HasFocusReader t m
       , HasDisplayRegion t m
       , HasLayout t m
       , MonadHold t m
       , MonadIO (Performable m)
       )
    => GHC.Client t
    -> LSP.Client t
    -> Notebook
    -> m (Event t Notebook)
notebook ghc lsp n = do
    cellEvents :: Event t [(Cell, CellEvent)] <- NonEmpty.toList <$$> (mergeList . toList) <$> for n.cellOrder (\cid -> (fromJust $ n.cells ^. at cid,) <$$> cell (fromJust $ n ^. #cells . at cid))
    cellUpdates :: Event t Notebook <- performEvent $ foldrM (uncurry $ handleCellEvent ghc lsp) n <$> cellEvents
    ghcUpdates :: Event t Notebook <-
        performEvent $
            flip (`alignEventWithMaybe` cellUpdates) ghc.onResponse $
                Just . \case
                    This n -> pure n
                    That r -> handleGhcResponse r n
                    These n r -> handleGhcResponse r n
    performEvent $
        flip (`alignEventWithMaybe` ghcUpdates) lsp.onResponse $
            Just . \case
                This n -> pure n
                That r -> handleLspResponse r n
                These n r -> handleLspResponse r n

handleCellEvent
    :: forall t m
     . MonadIO m
    => GHC.Client t
    -> LSP.Client t
    -> Cell
    -> CellEvent
    -> Notebook
    -> m Notebook
handleCellEvent _ _ Cell{uid} (UpdateCellCursor (moveCursor -> update)) n = do
    pure $ n & #cells . ix uid . #input %~ update
handleCellEvent _ lsp c@Cell{uid, input} (UpdateCellInput update) n = do
    liftIO $
        lsp.request
            LSP.ChangeDocument
                { identifier = n.identifier
                , range =
                    Just
                        LSP.Range
                            { _start = case update of
                                Insert _ -> position
                                DeleteLeft -> newPosition
                                DeleteRight -> position
                            , _end = case update of
                                Insert _ -> position
                                DeleteLeft -> position
                                DeleteRight ->
                                    let deltaLines = fromIntegral $ CZ.lines input - CZ.lines newZipper
                                     in position
                                            & if deltaLines == 0
                                                then LSP.character %~ (+ 1)
                                                else LSP.line %~ (+ deltaLines) >>> LSP.character .~ 0
                            }
                , contents = case update of
                    Insert t -> t
                    DeleteLeft -> ""
                    DeleteRight -> ""
                }
    pure $
        n
            & #cells . ix uid . #input .~ newZipper
            & filtered (const $ row /= newRow) %~ updateLineNumbers
  where
    row = firstLine c + CZ.row input
    col = CZ.col input
    position = LSP.Position (fromIntegral row) (fromIntegral col)
    newZipper = updateZipper update input
    newRow = firstLine c + CZ.row newZipper
    newCol = CZ.col newZipper
    newPosition = LSP.Position (fromIntegral newRow) (fromIntegral newCol)
    -- TODO: can we leverage a finger tree to do this automatically?
    updateLineNumbers :: Notebook -> Notebook
    updateLineNumbers n = flip (`foldl'` n) (Seq.zip n.cellOrder.after $ Seq.drop 1 n.cellOrder.after) $
        \n (c1, c2) ->
            n
                & #cells . ix c2 %~ #firstLine .~ lastLine (fromJust $ n ^. #cells . at c1) + 1
handleCellEvent ghc lsp c@Cell{uid, input} EvaluateCell n = do
    -- we send a new-line to LSP so it will be aware of the next cell
    when (shouldCreateNewCell n) $
        liftIO $
            lsp.request
                LSP.ChangeDocument
                    { identifier = n.identifier
                    , range =
                        Just
                            LSP.Range
                                { _start = LSP.Position{_line = fromIntegral $ lastLine c, _character = maxBound}
                                , _end = LSP.Position{_line = fromIntegral $ lastLine c, _character = maxBound}
                                }
                    , contents = "\n\n"
                    }
    let content = CZ.toText input
    liftIO $ ghc.request GHC.Evaluate{uid, content}
    maybeNewCell <-
        if shouldCreateNewCell n
            then Just <$> newCell (#firstLine .~ lastLine c + 1)
            else pure Nothing
    pure $
        n
            & updateNumbers
            & maybe id addCell maybeNewCell
            & #cells . ix uid %~ \c ->
                c
                    { output = Nothing
                    , error = Nothing
                    , Cell.disabled = True
                    , evaluated = True
                    }
  where
    cellEvaluated :: Notebook -> Bool
    cellEvaluated = maybe False evaluated . view (#cells . at uid)
    cellNotEvaluated :: Notebook -> Bool
    cellNotEvaluated = not . cellEvaluated
    isLastCell :: Notebook -> Bool
    isLastCell = (Just uid ==) . SZ.last . cellOrder
    shouldCreateNewCell :: Notebook -> Bool
    shouldCreateNewCell n = isLastCell n && cellNotEvaluated n
    updateNumbers :: Notebook -> Notebook
    updateNumbers n =
        n
            & #cells . traverse . filtered (not . evaluated) . #number %~ (+ 1)
            & #cells . ix uid . #number .~ n.nextCellNumber
            & #nextCellNumber %~ (+ 1)
handleCellEvent _ _ _ GoToPreviousCell n
    | havePrev n =
        pure $
            n
                & overCurrentCell (#disabled .~ True)
                & #cellOrder %~ SZ.back
                & overCurrentCell (#disabled .~ False)
    | otherwise = pure n
  where
    havePrev = not . null . SZ.before . cellOrder
handleCellEvent _ _ _ GoToNextCell n
    | haveNext n =
        pure $
            n
                & overCurrentCell (#disabled .~ True)
                & #cellOrder %~ SZ.forward
                & overCurrentCell (#disabled .~ False)
    | otherwise = pure n
  where
    haveNext = isJust . SZ.current . SZ.forward . cellOrder

handleGhcResponse
    :: forall m
     . MonadIO m
    => GHC.Response
    -> Notebook
    -> m Notebook
handleGhcResponse r =
    pure
        . ( #cells . ix r.uid %~ case r of
                GHC.FullResponse{content} -> #output ?~ content
                GHC.PartialResponse{content} -> #output %~ (Just . (<> content) . fromMaybe "")
                GHC.Error{error} -> #error ?~ tshow error
                GHC.EndResponse{} -> #disabled .~ False
          )

handleLspResponse
    :: forall m
     . MonadIO m
    => LSP.Response
    -> Notebook
    -> m Notebook
handleLspResponse LSP.DocumentContents{..} = #contents .~ contents >>> pure
handleLspResponse LSP.Diagnostics{..} = clearDiagnostics >>> setDiagnostics >>> pure
  where
    clearDiagnostics :: Notebook -> Notebook
    clearDiagnostics = #cells . traverse . #diagnostics .~ []
    setDiagnostics :: Notebook -> Notebook
    setDiagnostics = foldr (.) id $ setDiagnostic <$> diagnostics
    setDiagnostic :: LSP.Diagnostic -> Notebook -> Notebook
    setDiagnostic d = #cells . traverse . filtered (hasDiagnostic d) %~ #diagnostics %~ (d :)
    hasDiagnostic :: LSP.Diagnostic -> Cell -> Bool
    hasDiagnostic (diagnosticLine -> l) c = firstLine c <= l && lastLine c >= l
handleLspResponse LSP.Completions{} = pure
