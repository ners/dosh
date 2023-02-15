module Dosh.Cell where

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
import Reflex.Vty
import Data.Maybe (isJust)

data Cell = Cell
    { number :: Int
    , input :: Text
    , output :: Maybe Text
    , error :: Maybe Text
    , disabled :: Bool
    , evaluated :: Bool
    }
    deriving stock (Show, Generic)

newCell :: Int -> Cell
newCell number =
    Cell
        { number
        , input = ""
        , output = Nothing
        , error = Nothing
        , disabled = False
        , evaluated = False
        }

data CellEvent
    = UpdateCellInput Text
    | EvaluateCell Text
    deriving stock (Show)

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
       , MonadFix (Performable m)
       )
    => Cell
    -> m (Event t CellEvent)
cell c = do
    let inPrompt = " In[" <> tshow c.number <> "]: "
    let outPrompt = "Out[" <> tshow c.number <> "]: "
    inputEvent <- grout (fixed $ pure $ max 1 $ length $ Text.lines c.input) $ row $ do
        grout (fixed $ pure $ Text.length inPrompt) $ text $ pure inPrompt
        if c.disabled
            then do
                grout flex $ text $ pure c.input
                pure never
            else do
                TextInput{..} <- grout flex $ textInput def{_textInputConfig_initialValue = TZ.fromText c.input}
                -- let updateInput :: Event t CellEvent
                --    updateInput = UpdateCellInput <$> updated _textInput_value
                evaluate :: Event t CellEvent <- EvaluateCell <$$> tagPromptlyDyn _textInput_value <$> enterPressed
                pure $ leftmost [evaluate]
    when (isJust c.output || isJust c.error) $
        grout (fixed $ pure $ max 1 $ maybe 0 (length . Text.lines) (c.output <> c.error)) $ row $ do
            grout (fixed $ pure $ Text.length outPrompt) $ text $ pure outPrompt
            grout flex $ do
                mapM_ (text . pure) c.output
                mapM_ (text . pure) c.error
    pure inputEvent
