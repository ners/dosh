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
import Graphics.Vty qualified as V
import Reflex
import Reflex.Vty
import Reflex.Vty.Widget.Input.Code
import Data.Functor (($>))

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
    newLine <- altEnterPressed
    i :: Event t VtyEvent <- Reflex.Vty.input
    dh :: Dynamic t Int <- displayHeight
    let updateEvent :: Event t (CodeZipper -> CodeZipper)
        updateEvent =
            mergeWith
                (.)
                [ newLine $> highlightZipper . updateZipper (TZ.insertChar '\n')
                , uncurry (updateCodeZipper 4) <$> attach (current dh) i
                ]
    codeZipper <- foldDyn ($) (codeZipperFromText "Haskell" c.input) updateEvent
    inputEvent <- grout (fixed $ length . highlighted <$> codeZipper) $ row $ do
        grout (fixed $ pure $ Text.length inPrompt) $ text $ pure inPrompt
        if c.disabled
            then do
                grout flex $ text $ pure c.input
                pure never
            else do
                CodeInput{..} <-
                    grout flex $
                        codeInput
                            def
                                { _codeInputConfig_value = Just codeZipper
                                }
                EvaluateCell <$$> tagPromptlyDyn _codeInput_value <$> enterPressed
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
    --grout (fixed $ pure 1) $ row $ text $ current $ tshow . zipper <$> codeZipper
    pure inputEvent

redText :: forall t m. (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => Behavior t Text -> m ()
redText = richText RichTextConfig{_richTextConfig_attributes = pure $ V.withForeColor V.currentAttr V.red}
