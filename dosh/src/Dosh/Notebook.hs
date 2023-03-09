{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.Notebook where

import Control.Lens
import Control.Monad.Fix
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Sequence.Zipper (SeqZipper (..))
import Data.Sequence.Zipper qualified as SZ
import Data.Text.CodeZipper qualified as CZ
import Data.Text.IO qualified as Text
import Data.These (These (..))
import Data.Traversable (for)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Development.IDE (Pretty (pretty), WithPriority (..))
import Development.IDE.Main (Log (..))
import Dosh.Cell
import Dosh.Cell qualified as Cell
import Dosh.GHC.Client qualified as GHC
import Dosh.LSP.Client qualified as LSP
import Dosh.LSP.Document (ChunkMetadata, ChunkType (..), Document, newDocument)
import Dosh.LSP.Document qualified as Document
import Dosh.Prelude
import Dosh.Util
import GHC.Exts (IsList (toList))
import Language.LSP.Test qualified as LSP
import Language.LSP.Types qualified as LSP
import Reflex hiding (Query, Response)
import Reflex.Vty hiding (Query, Response)

data Notebook = Notebook
    { uid :: UUID
    , cells :: HashMap UUID Cell
    , cellOrder :: SeqZipper UUID
    , nextCellNumber :: Int
    , disabled :: Bool
    , document :: Document
    }
    deriving stock (Generic, Show)

newNotebook :: MonadIO m => m Notebook
newNotebook = do
    uid <- liftIO UUID.nextRandom
    Notebook
        { uid
        , cells = mempty
        , cellOrder = mempty
        , nextCellNumber = 1
        , disabled = False
        , document = newDocument $ LSP.Uri "file:///home/ners/Projects/dosh/app/Main.hs"
        }
        & createCell (#disabled .~ False)

-- | Create a new cell with a random UUID.
createCell :: MonadIO m => (Cell -> Cell) -> Notebook -> m Notebook
createCell f n = (n &) . createCell' . const . f <$> newCell n.nextCellNumber

{- | Create a new cell with an empty UUID.
 You almost certainly want to call this with a transformation that sets the UUID.
-}
createCell' :: (Cell -> Cell) -> Notebook -> Notebook
createCell' f n =
    n
        & #cells . at c.uid ?~ c
        & #cellOrder %~ (<> SZ.singleton c.uid)
        & #document . #chunks %~ (<> SZ.singleton chunk)
  where
    c = f def{number = n.nextCellNumber}
    chunk =
        Document.ChunkMetadata
            { cellId = c.uid
            , chunkIndex = 0
            , chunkType = Expression
            , firstLine = firstLine c
            , lastLine = lastLine c
            }

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
    cellEvents :: Event t [(Cell, CellEvent)] <- NonEmpty.toList <$$> (mergeList . GHC.Exts.toList) <$> for n.cellOrder (\cid -> (fromJust $ n.cells ^. at cid,) <$$> cell (fromJust $ n ^. #cells . at cid))
    cellUpdates :: Event t Notebook <- performEvent $ foldrM (uncurry $ handleCellEvent ghc lsp) n <$> cellEvents
    ghcUpdates :: Event t Notebook <-
        performEvent $
            flip (`alignEventWithMaybe` cellUpdates) ghc.onResponse $
                Just . \case
                    This n -> pure n
                    That r -> handleGhcResponse r n
                    These n r -> handleGhcResponse r n
    lspUpdates :: Event t Notebook <-
        performEvent $
            flip (`alignEventWithMaybe` ghcUpdates) lsp.onResponse $
                Just . \case
                    This n -> pure n
                    That r -> handleLspResponse r n
                    These n r -> handleLspResponse r n
    lspErrors <-
        performEvent $
            flip (`alignEventWithMaybe` lspUpdates) lsp.onError $
                Just . \case
                    This n -> pure n
                    That e -> pure $ n & #document . #error %~ (<> tshow e <> "\n")
                    These n e -> pure $ n & #document . #error %~ (<> tshow e <> "\n")
    performEvent $
        flip (`alignEventWithMaybe` lspErrors) lsp.onLog $
            Just . \case
                This n -> pure n
                That l -> do
                    liftIO $ Text.appendFile "hls-log.log" $ l.payload <> "\n"
                    pure n
                These n l -> do
                    liftIO $ Text.appendFile "hls-log.log" $ l.payload <> "\n"
                    pure n

handleCellEvent
    :: forall t m
     . MonadIO m
    => GHC.Client t
    -> LSP.Client t
    -> Cell
    -> CellEvent
    -> Notebook
    -> m Notebook
handleCellEvent _ _ Cell{uid, firstLine, input} (UpdateCellCursor (moveCursor -> update)) n = do
    let newZipper = update input
        newRow = firstLine + CZ.row newZipper
    pure $
        n
            & #cells . ix uid . #input .~ newZipper
            & #document %~ Document.goToLine newRow
handleCellEvent _ lsp c@Cell{uid, input} (UpdateCellInput update) n = do
    -- TODO: this is buggy!
    -- TODO: make LSP updates more efficient by sending smaller ranges
    liftIO $
        lsp.request
            LSP.ChangeDocument
                { uri = n.document.uri
                , range =
                    LSP.Range
                        { _start = LSP.Position{_line = fromIntegral $ firstLine c, _character = 0}
                        , _end = LSP.Position{_line = fromIntegral $ lastLine c, _character = 0}
                        }
                , text = newZipperT
                }
    pure $
        n
            & #cells . ix uid . #input .~ newZipper
            & filtered (const $ row /= newRow)
                %~ ( updateLineNumbers
                        >>> #document . #chunks . #after %~ updateChunkLines
                   )
  where
    row = firstLine c + CZ.row input
    col = CZ.col input
    newZipper = updateZipper update input
    newZipperT = CZ.toText newZipper
    newRow = firstLine c + CZ.row newZipper
    newCol = CZ.col newZipper
    -- TODO: can we leverage a finger tree to do this automatically?
    updateLineNumbers :: Notebook -> Notebook
    updateLineNumbers n = flip (`foldl'` n) (Seq.zip n.cellOrder.after $ Seq.drop 1 n.cellOrder.after) $
        \n (c1, c2) ->
            n
                & #cells . ix c2 %~ #firstLine .~ lastLine (fromJust $ n ^. #cells . at c1) + 1
    updateChunkLines :: Seq ChunkMetadata -> Seq ChunkMetadata
    updateChunkLines =
        overHead (#firstLine %~ min newRow >>> #lastLine %~ (+ (newRow - row)))
            >>> overTail (#firstLine %~ (+ (newRow - row)) >>> (#lastLine %~ (+ (newRow - row))))
    overHead :: forall a. (a -> a) -> Seq a -> Seq a
    overHead = flip Seq.adjust 0
    overTail :: forall a. (a -> a) -> Seq a -> Seq a
    overTail = (dropping 1 traverse %~)
handleCellEvent ghc lsp c@Cell{uid, input} EvaluateCell n = do
    -- we send a new-line to LSP so it will be aware of the next cell
    liftIO $
        lsp.request
            LSP.ChangeDocument
                { uri = n.document.uri
                , range =
                    LSP.Range
                        { _start = LSP.Position{_line = fromIntegral $ lastLine c + 1, _character = 0}
                        , _end = LSP.Position{_line = fromIntegral $ lastLine c + 1, _character = 0}
                        }
                , text = "\n\n"
                }
    let content = CZ.toText input
    liftIO $ ghc.request GHC.Evaluate{uid, content}
    newCellUid <- liftIO UUID.nextRandom
    pure $
        n
            & updateNumbers
            & filtered shouldCreateNewCell %~ createNewCell newCellUid
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
    createNewCell :: UUID -> Notebook -> Notebook
    createNewCell uid = createCell' $ #uid .~ uid >>> #firstLine .~ lastLine c + 1
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
                & #document . #chunks %~ SZ.back
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
                & #document . #chunks %~ SZ.forward
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
handleLspResponse LSP.DocumentContents{..} = pure . (#document . #contents .~ contents)
handleLspResponse LSP.Diagnostics{..} = pure . (#document . #diagnostics .~ diagnostics)