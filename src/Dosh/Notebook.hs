{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.Notebook where

import Control.Lens
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Dosh.Cell
import Dosh.Server (Query (..), Response (..), Server (..))
import Dosh.Util
import GHC.Generics (Generic)
import Reflex hiding (Query, Response)
import Reflex.Vty hiding (Query, Response)

data Notebook = Notebook
    { cells :: Map Int Cell
    , nextCellId :: Int
    , nextCellNumber :: Int
    }
    deriving stock (Show, Generic)

newNotebook :: Notebook
newNotebook =
    Notebook
        { cells = Map.singleton 1 (newCell 1)
        , nextCellId = 2
        , nextCellNumber = 2
        }

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
    => Server t
    -> Notebook
    -> m (Event t Notebook)
notebook io n = do
    cellEvent :: Event t (Int, CellEvent) <- minmost <$> mapM cell n.cells
    cellEventHandled <- performEvent $ handleCellEvent io n <$> cellEvent
    ioResponseHandled <- performEvent $ handleIoResponse n <$> io.response
    pure $ leftmost [cellEventHandled, ioResponseHandled]

handleCellEvent
    :: MonadIO m
    => Server t
    -> Notebook
    -> (Int, CellEvent)
    -> m Notebook
handleCellEvent _ n (id, UpdateCellInput t) = pure $ n & #cells . ix id . #input .~ t
handleCellEvent io n (id, EvaluateCell content) = do
    liftIO $ io.query Query{..}
    pure $
        n
            & #cells . ix id . #disabled .~ True
            & #cells . ix id . #input .~ content
            & #cells . ix id . #output .~ Nothing

handleIoResponse :: MonadIO m => Notebook -> Response -> m Notebook
handleIoResponse n r@EndResponse{} = pure $ n & #cells . ix r.id . #disabled .~ False
handleIoResponse n r@FullResponse{} = pure $ n & #cells . ix r.id . #output ?~ r.content
handleIoResponse n r@PartialResponse{} = pure $ n & #cells . ix r.id . #output %~ (Just . (<> r.content) . fromMaybe "")
