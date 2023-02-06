module Dosh.Cell where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Zipper qualified as TZ
import Dosh.Util
import GHC.Generics (Generic)
import Reflex
import Reflex.ExternalRef
import Reflex.Vty

data Cell = Cell
    { number :: Int
    , input :: Text
    , output :: Maybe Text
    }
    deriving stock (Show, Generic)

newCell :: Int -> Cell
newCell number =
    Cell
        { number
        , input = ""
        , output = Nothing
        }

evaluateCell
    :: forall t m
     . ( Reflex t
       , PerformEvent t m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadHold t (Performable m)
       )
    => ExternalRef t Text
    -> ExternalRef t Text
    -> Event t Cell
    -> m (Event t Cell)
evaluateCell i o ec = do
    evaluate <- performEvent $ eval <$> ec
    switchHold never evaluate
  where
    eval :: Cell -> Performable m (Event t Cell)
    eval c = do
        writeExternalRef i c.input
        et :: Event t Text <- externalRefEvent o
        pure $ et <&> \t -> c & #output ?~ t

cell
    :: forall t m
     . ( Reflex t
       , PerformEvent t m
       , HasFocusReader t m
       , HasImageWriter t m
       , HasInput t m
       , HasLayout t m
       , HasDisplayRegion t m
       , MonadFix m
       , HasTheme t m
       , MonadHold t m
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
        evaluated :: Event t Cell <- evaluateCell i o onEnter
        pure $ leftmost [evaluated, updated edited]
    forM_ c.output $ \(output :: Text) -> do
        let ps1 = "Out[" <> tshow c.number <> "]: "
        grout (fixed $ pure $ Text.length ps1) $ text $ pure ps1
        grout (fixed $ pure 1) $ row $ do
            grout flex $ text $ pure output
    pure update
