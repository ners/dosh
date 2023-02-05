module MyLib (someFunc) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Zipper qualified as TZ
import GHC.Generics (Generic)
import Optics
import Reflex
import Reflex.Vty
import System.IO.Unsafe (unsafePerformIO)
import Util

data Cell = Cell
    { number :: Int
    , input :: Text
    , output :: Maybe Text
    }
    deriving stock (Show, Generic)

newCell :: Int -> Cell
newCell number = Cell{input = "", output = Nothing, ..}

evaluateCell :: Cell -> IO Cell
evaluateCell c = pure $ c & #output ?~ c.input

cell
    :: forall t m
     . ( MonadIO m
       , Reflex t
       , HasInput t m
       , MonadFix m
       , HasImageWriter t m
       , HasTheme t m
       , HasFocusReader t m
       , HasDisplayRegion t m
       , HasLayout t m
       , MonadHold t m
       )
    => Cell
    -> m (Event t Cell)
cell c = do
    update <- grout (fixed $ pure 1) $ row $ do
        let ps1 = "[" <> tshow c.number <> "]: "
        grout (fixed $ pure $ Text.length ps1) $ text $ pure ps1
        TextInput{..} <- grout flex $ textInput def{_textInputConfig_initialValue = TZ.fromText c.input}
        let edited = set #input <$> _textInput_value <*> pure c
        onEnter :: Event t Cell <- tagPromptlyDyn edited <$> enterPressed
        let evaluated = unsafePerformIO . evaluateCell <$> onEnter
        pure $ leftmost [evaluated, updated edited]
    forM_ c.output $ \(o :: Text) -> do
        grout (fixed $ pure 1) $ row $ do
            grout flex $ text $ pure o
    pure update

data Notebook = Notebook
    { cells :: Map Int Cell
    , nextCellNumber :: Int
    }
    deriving stock (Show, Generic)

newNotebook :: Notebook
newNotebook = Notebook{cells = Map.singleton 1 (newCell 1), nextCellNumber = 2}

notebook
    :: ( MonadIO m
       , Reflex t
       , HasInput t m
       , MonadFix m
       , HasImageWriter t m
       , HasTheme t m
       , HasFocusReader t m
       , HasDisplayRegion t m
       , HasLayout t m
       , MonadHold t m
       )
    => Notebook
    -> m (Event t Notebook)
notebook n = do
    cellUpdate :: Event t (Int, Cell) <- leftmost . functorMapToList <$> mapM cell n.cells
    pure $ cellUpdate <&> (\(i, c) -> n & #cells %~ Map.insert i c)

someFunc :: IO ()
someFunc = mainWidget $ initManager_ $ mdo
    dn <- holdDyn newNotebook u
    n <- sample $ current dn
    u <- notebook n
    grout flex $ text $ tshow <$> current dn
    void <$> ctrldPressed
