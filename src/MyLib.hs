module MyLib (someFunc) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Zipper qualified as TZ
import GHC.Generics (Generic)
import Optics
import Reflex
import Reflex.ExternalRef
import Reflex.Vty
import Util

data Cell = Cell
    { number :: Int
    , input :: Text
    , output :: Maybe Text
    }
    deriving stock (Show, Generic)

newCell :: Int -> Cell
newCell number =
    Cell
        { input = ""
        , output = Nothing
        , ..
        }

evaluateCell
    :: forall t m
     . ( Reflex t
       , MonadIO m
       , MonadHold t m
       )
    => ExternalRef t Text
    -> ExternalRef t Text
    -> Cell
    -> m (Dynamic t Cell)
evaluateCell i o c = do
    writeExternalRef i c.input
    externalRefDynamic o <&&> \o' -> c & #output ?~ o'

cell
    :: forall t m
     . ( MonadIO m
       , Reflex t
       , HasFocusReader t m
       , HasImageWriter t m
       , HasInput t m
       , HasLayout t m
       , HasDisplayRegion t m
       , MonadFix m
       , HasTheme t m
       , MonadHold t m
       , PerformEvent t m
       , MonadIO (Performable m)
       , MonadHold t (Performable m)
       )
    => ExternalRef t Text
    -> ExternalRef t Text
    -> Cell
    -> m (Event t Cell)
cell i o c = do
    update <- grout (fixed $ pure 1) $ row $ do
        let ps1 = "In[" <> tshow c.number <> "]: "
        grout (fixed $ pure $ Text.length ps1) $ text $ pure ps1
        TextInput{..} <- grout flex $ textInput def{_textInputConfig_initialValue = TZ.fromText c.input}
        let edited = set #input <$> _textInput_value <*> pure c
        onEnter :: Event t Cell <- tagPromptlyDyn edited <$> enterPressed
        evaluated :: Event t (Event t Cell) <- updated <$$> performEvent (evaluateCell i o <$> onEnter)
        evaluated' <- switchHold never evaluated
        pure $ leftmost [evaluated', updated edited]
    forM_ c.output $ \(output :: Text) -> do
        let ps1 = "Out[" <> tshow c.number <> "]: "
        grout (fixed $ pure $ Text.length ps1) $ text $ pure ps1
        grout (fixed $ pure 1) $ row $ do
            grout flex $ text $ pure output
    pure update

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
       , MonadIO (Performable m)
       , PerformEvent t m
       , MonadHold t (Performable m)
       )
    => ExternalRef t Text
    -> ExternalRef t Text
    -> Notebook
    -> m (Event t Notebook)
notebook i o n = do
    cellUpdate :: Event t (Int, Cell) <- leftmost . functorMapToList <$> mapM (cell i o) n.cells
    pure $ cellUpdate <&> (\(number, c) -> n & #cells %~ Map.insert number c)

echoServer
    :: ExternalRef t Text
    -> ExternalRef t Text
    -> IO ()
echoServer i o = forever $ do
    incomingText <- readExternalRef i
    forM_ (Text.inits incomingText) $ \prefix -> do
        threadDelay 100_000
        writeExternalRef o prefix

someFunc :: IO ()
someFunc = mainWidget $ do
    i <- newExternalRef ""
    o <- newExternalRef ""
    _ <- liftIO $ forkIO $ echoServer i o
    initManager_ $ mdo
        dn <- holdDyn newNotebook u
        u <- dyn (notebook i o <$> dn) >>= switchHold never
        grout flex $ text $ tshow <$> current dn
        void <$> ctrldPressed
