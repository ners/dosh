{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.Notebook where

import Control.Lens
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Dosh.Cell
import Dosh.GHC.Client qualified as GHC
import Dosh.Util
import GHC.Generics (Generic)
import Reflex hiding (Query, Response)
import Reflex.Vty hiding (Query, Response)

data Notebook = Notebook
    { cells :: Map Int Cell
    , nextCellId :: Int
    , nextCellNumber :: Int
    , disabled :: Bool
    }
    deriving stock (Show, Generic)

newNotebook :: Notebook
newNotebook =
    Notebook
        { cells = Map.singleton 1 (newCell 1)
        , nextCellId = 2
        , nextCellNumber = 2
        , disabled = False
        }

createCell :: Notebook -> Notebook
createCell n =
    n
        & #cells . at n.nextCellId ?~ newCell n.nextCellNumber
        & #nextCellId %~ (+ 1)
        & #nextCellNumber %~ (+ 1)

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
    cellEvents :: Event t (Map Int CellEvent) <- mergeMap <$> mapM cell n.cells
    cellUpdates <- performEvent $ Map.traverseWithKey (handleCellEvent ghc) <$> cellEvents
    let ghcUpdates :: Event t (Map Int (Cell -> Cell))
        ghcUpdates = uncurry Map.singleton . handleGhcResponse <$> ghc.onResponse
        allUpdates = mergeWith (Map.unionWith (.)) [cellUpdates, ghcUpdates]
    pure $ (n &) . over #cells . transformMap <$> allUpdates

handleCellEvent
    :: forall t m
     . MonadIO m
    => GHC.Client t
    -> Int
    -> CellEvent
    -> m (Cell -> Cell)
handleCellEvent _ _ (UpdateCellInput content) = pure $ #input .~ content
handleCellEvent ghc id (EvaluateCell content) = do
    liftIO $ ghc.query GHC.Query{..}
    handleCellEvent ghc id (UpdateCellInput content) <&> \updateInput cell ->
        updateInput cell
            & #output .~ Nothing
            & #error .~ Nothing
            & #disabled .~ True
            & filtered evaluated %~ #number %~ (+ 1)
            & #evaluated .~ True

handleGhcResponse :: GHC.Response -> (Int, Cell -> Cell)
handleGhcResponse r = (r.id,) $ case r of
    GHC.FullResponse{content} -> #output ?~ content
    GHC.PartialResponse{content} -> #output %~ (Just . (<> content) . fromMaybe "")
    GHC.Error{error} -> #error ?~ tshow error
    GHC.EndResponse{} -> #disabled .~ False
