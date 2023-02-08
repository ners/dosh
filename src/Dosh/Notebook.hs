module Dosh.Notebook where

import Control.Lens
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Dosh.Cell
import Dosh.Util
import GHC.Generics (Generic)
import Reflex
import Reflex.Vty

data Notebook = Notebook
    { cells :: Map Int Cell
    , nextCellNumber :: Int
    }
    deriving stock (Show, Generic)

newNotebook :: Notebook
newNotebook =
    Notebook
        { cells = Map.singleton 1 (newCell 1)
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
    => IoServer t
    -> Notebook
    -> m (Event t Notebook)
notebook io n = do
    cellEvent :: Event t (Int, CellEvent) <- minmost <$> mapM cell n.cells
    cellEventHandled <- performEvent $ handleCellEvent io n <$> cellEvent
    ioResponseHandled <- performEvent $ handleIoResponse n <$> io.response
    pure $ leftmost [cellEventHandled, ioResponseHandled]

handleCellEvent
    :: MonadIO m
    => IoServer t
    -> Notebook
    -> (Int, CellEvent)
    -> m Notebook
handleCellEvent _ n (i, UpdateCellInput t) = pure $ n & #cells . ix i . #input .~ t
handleCellEvent io n (i, EvaluateCell) = do
    liftIO $ io.query (i, n ^. #cells . ix i . #input)
    pure $
        n
            & #cells . ix i . #disabled .~ True
            & #cells . ix i . #output .~ Nothing

handleIoResponse :: MonadIO m => Notebook -> (Int, Text) -> m Notebook
handleIoResponse n (i, t) = pure $ n & #cells . ix i . #output %~ (Just . (<> t) . fromMaybe "")
