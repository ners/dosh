module Dosh.Cell where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Zipper qualified as TZ
import GHC.Generics (Generic)
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
