module Dosh.Cell where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Generics.Labels ()
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Zipper (DisplayLines (..), Span (..))
import Dosh.Util
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Reflex
import Reflex.Vty
import Reflex.Vty.Widget.Input.Code

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
    let errPrompt = "Err[" <> tshow c.number <> "]: "
    (inputEvent, i) <- grout (fixed $ pure $ (1 +) $ Text.count "\n" $ c.input) $ row $ do
        grout (fixed $ pure $ Text.length inPrompt) $ text $ pure inPrompt
        if c.disabled
            then do
                grout flex $ text $ pure c.input
                pure (never, Nothing)
            else do
                i@CodeInput{..} <- grout flex $ codeInput def{_codeInputConfig_initialValue = codeZipperFromText "Haskell" c.input}
                let updateInput :: Event t CellEvent
                    updateInput = UpdateCellInput <$> updated _codeInput_value
                evaluate :: Event t CellEvent <- EvaluateCell <$$> tagPromptlyDyn _codeInput_value <$> enterPressed
                pure (leftmost [evaluate], Just i)
    forM_ c.output $ \out ->
        grout (fixed $ pure $ 1 + Text.count "\n" out) $
            row $ do
                grout (fixed $ pure $ Text.length outPrompt) $ text $ pure outPrompt
                grout flex $ text $ pure out
    forM_ c.error $ \err ->
        grout (fixed $ pure $ 1 + Text.count "\n" err) $
            row $ do
                grout (fixed $ pure $ Text.length errPrompt) $ text $ pure errPrompt
                grout flex $ redText $ pure err
    -- forM_ i $ \j -> do
    --    grout (fixed $ pure 1) $ row $ text $ current $ tshow <$> _codeInput_zipper j
    --    grout (fixed $ pure 1) $ row $ text $ current $ tshow . ((\(Span _ t) -> t) <$$>) . _displayLines_spans <$> j._codeInput_displayLines
    pure inputEvent

redText :: forall t m. (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => Behavior t Text -> m ()
redText = richText RichTextConfig{_richTextConfig_attributes = pure $ V.withForeColor V.currentAttr V.red}
