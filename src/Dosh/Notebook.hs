module Dosh.Notebook where

import Control.Lens
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Dosh.Cell
import GHC.Generics (Generic)
import Reflex
import Reflex.ExternalRef
import Reflex.Vty
import Util

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
       )
    => ExternalRef t Text
    -> ExternalRef t Text
    -> Notebook
    -> m (Event t Notebook)
notebook i o n = do
    cellUpdate :: Event t (Int, Cell) <- leftmost . functorMapToList <$> mapM (cell i o) n.cells
    pure $ cellUpdate <&> (\(number, c) -> n & #cells %~ Map.insert number c)
