{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.Notebook where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence.Zipper (SeqZipper)
import Data.Sequence.Zipper qualified as SZ
import Data.Text.CodeZipper qualified as CZ
import Data.Traversable (for)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Dosh.Cell
import Dosh.Cell qualified as Cell
import Dosh.GHC.Client qualified as GHC
import Dosh.Util
import GHC.Data.Maybe (isJust)
import GHC.Exts (IsList (toList))
import GHC.Generics (Generic)
import Reflex hiding (Query, Response)
import Reflex.Vty hiding (Query, Response)

data Notebook = Notebook
    { cells :: HashMap UUID Cell
    , cellOrder :: SeqZipper UUID
    , nextCellNumber :: Int
    , disabled :: Bool
    }
    deriving stock (Generic, Eq, Show)

newNotebook :: MonadIO m => m Notebook
newNotebook =
    Notebook
        { cells = mempty
        , cellOrder = mempty
        , nextCellNumber = 1
        , disabled = False
        }
        & createCell (#disabled .~ False)

-- | Create a new cell with a random UUID.
createCell :: MonadIO m => (Cell -> Cell) -> Notebook -> m Notebook
createCell f n = (n &) . createCell' . const . f <$> newCell n.nextCellNumber

-- | Create a new cell with an empty UUID.
-- You almost certainly want to call this with a transformation that sets the UUID.
createCell' :: (Cell -> Cell) -> Notebook -> Notebook
createCell' f n =
    n
        & #cells . at c.uid ?~ c
        & #cellOrder %~ (<> SZ.singleton c.uid)
  where
    c = f def{number = n.nextCellNumber}

currentCellUid :: Notebook -> Maybe UUID
currentCellUid n = SZ.current n.cellOrder

overUid :: UUID -> (Cell -> Cell) -> Notebook -> Notebook
overUid uid f = #cells . ix uid %~ f

currentCell :: Notebook -> Maybe Cell
currentCell n = currentCellUid n >>= \cid -> n ^. #cells . at cid

overCurrentCell :: (Cell -> Cell) -> Notebook -> Notebook
overCurrentCell f n = flip (maybe n) (currentCellUid n) $ \uid -> overUid uid f n

notebook
    :: forall t m
     . ( Reflex t
       , PerformEvent t m
       , HasInput t m
       , MonadFix m
       , HasImageWriter t m
       , HasTheme t m
       , HasFocusReader t m
       , HasDisplayRegion t m
       , HasLayout t m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadHold t (Performable m)
       , MonadFix (Performable m)
       , PostBuild t m
       , TriggerEvent t m
       )
    => GHC.Client t
    -> Notebook
    -> m (Event t Notebook)
notebook ghc n = do
    cellEvents :: Event t [(UUID, CellEvent)] <- NonEmpty.toList <$$> (mergeList . toList) <$> for n.cellOrder (\cid -> (cid,) <$$> cell (fromJust $ n ^. #cells . at cid))
    cellUpdates :: Event t (Notebook -> Notebook) <- foldr (.) id <$$> performEvent (traverse (uncurry $ handleCellEvent ghc) <$> cellEvents)
    let ghcUpdates :: Event t (Notebook -> Notebook)
        ghcUpdates = (\(id, u) -> #cells . ix id %~ u) . handleGhcResponse <$> ghc.onResponse
        allUpdates :: Event t (Notebook -> Notebook)
        allUpdates = mergeWith (.) [cellUpdates, ghcUpdates]
    pure $ allUpdates <&> ($ n)

handleCellEvent
    :: forall t m
     . MonadIO m
    => GHC.Client t
    -> UUID
    -> CellEvent
    -> m (Notebook -> Notebook)
handleCellEvent _ uid (UpdateCellInput update) = pure $ #cells . ix uid . #input %~ update
handleCellEvent ghc uid (EvaluateCell content) = do
    liftIO $ ghc.query GHC.Query{uid, content = CZ.toText content}
    newCellUid <- liftIO UUID.nextRandom
    pure $
        updateNumbers
            >>> filtered shouldCreateNewCell %~ createNewCell newCellUid
            >>> #cells . ix uid %~ \c -> c{output = Nothing, error = Nothing, Cell.disabled = True, evaluated = True}
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
    createNewCell uid = createCell' $ #uid .~ uid
    updateNumbers :: Notebook -> Notebook
    updateNumbers n =
        n
            & #cells . traverse . filtered (not . evaluated) . #number %~ (+ 1)
            & #cells . ix uid . #number .~ n.nextCellNumber
            & #nextCellNumber %~ (+ 1)
handleCellEvent _ _ GoToPreviousCell = do
    pure $
        ifHavePrev $
            overCurrentCell (#disabled .~ True)
                >>> #cellOrder %~ SZ.back
                >>> overCurrentCell (#disabled .~ False)
  where
    havePrev = not . null . SZ.before . cellOrder
    ifHavePrev f n = if havePrev n then f n else n
handleCellEvent _ _ GoToNextCell =
    pure $
        ifHaveNext $
            overCurrentCell (#disabled .~ True)
                >>> #cellOrder %~ SZ.forward
                >>> overCurrentCell (#disabled .~ False)
  where
    haveNext = isJust . SZ.current . SZ.forward . cellOrder
    ifHaveNext f n = if haveNext n then f n else n

handleGhcResponse :: GHC.Response -> (UUID, Cell -> Cell)
handleGhcResponse r = (r.uid,) $ case r of
    GHC.FullResponse{content} -> #output ?~ content
    GHC.PartialResponse{content} -> #output %~ (Just . (<> content) . fromMaybe "")
    GHC.Error{error} -> #error ?~ tshow error
    GHC.EndResponse{} -> #disabled .~ False
