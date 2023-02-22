{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.Cell where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Default (Default)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.CodeZipper (CodeZipper)
import Data.Text.CodeZipper qualified as CZ
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Dosh.Util
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Reflex
import Reflex.Vty
import Reflex.Vty.Widget.Input.Code
import Skylighting (TokenType)

data Cell = Cell
    { uid :: UUID
    , number :: Int
    , input :: CodeZipper TokenType
    , output :: Maybe Text
    , error :: Maybe Text
    , disabled :: Bool
    , evaluated :: Bool
    }
    deriving stock (Generic, Eq, Show)

newCell :: MonadIO m => Int -> m Cell
newCell number = do
    uid <- liftIO UUID.nextRandom
    pure $ def{uid, number}

instance Default Cell where
    def =
        Cell
            { uid = UUID.nil
            , number = 0
            , input = CZ.empty{CZ.language = "Haskell"}
            , output = Nothing
            , error = Nothing
            , disabled = True
            , evaluated = False
            }

data CellEvent
    = UpdateCellInput (CodeZipper TokenType -> CodeZipper TokenType)
    | EvaluateCell (CodeZipper TokenType)
    | GoToPreviousCell
    | GoToNextCell

cell
    :: forall t m
     . ( Reflex t
       , PerformEvent t m
       , TriggerEvent t m
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
    (cellEvent, triggerCellEvent) <- newTriggerEvent @t @m @CellEvent
    let inPrompt = mconcat [if c.evaluated then "*" else " ", "In[", tshow c.number, "]: "]
    let outPrompt = "Out[" <> tshow c.number <> "]: "
    let errPrompt = "Err[" <> tshow c.number <> "]: "
    unless c.disabled $ do
        vtyInput :: Event t VtyEvent <- Reflex.Vty.input
        dh :: Dynamic t Int <- displayHeight
        let updateZipper = triggerCellEvent . UpdateCellInput
        void $
            performEvent $
                current dh `attach` vtyInput <&> \(dh, ev) -> liftIO $ case ev of
                    -- Delete character in zipper
                    V.EvKey V.KBS [] -> updateZipper CZ.deleteLeft
                    V.EvKey V.KDel [] -> updateZipper CZ.deleteRight
                    -- Movement in zipper and between cells
                    V.EvKey V.KUp [] ->
                        if null c.input.linesBefore
                            then triggerCellEvent GoToPreviousCell
                            else updateZipper CZ.up
                    V.EvKey V.KDown [] ->
                        if null c.input.linesAfter
                            then triggerCellEvent GoToNextCell
                            else updateZipper CZ.down
                    V.EvKey V.KLeft [] -> updateZipper CZ.left
                    V.EvKey V.KRight [] -> updateZipper CZ.right
                    V.EvKey V.KHome [] -> updateZipper CZ.home
                    V.EvKey V.KEnd [] -> updateZipper CZ.end
                    V.EvKey V.KPageUp [] -> updateZipper $ CZ.upN dh
                    V.EvKey V.KPageDown [] -> updateZipper $ CZ.downN dh
                    -- Insert characters into zipper
                    V.EvKey V.KEnter [] -> triggerCellEvent $ EvaluateCell c.input
                    V.EvKey V.KEnter [V.MMeta] -> updateZipper $ CZ.insertChar '\n'
                    V.EvKey (V.KChar '\t') [] -> updateZipper $ CZ.insert $ Text.replicate (1 + ((CZ.col c.input + 1) `mod` 4)) " "
                    V.EvKey (V.KChar k) [] -> updateZipper $ CZ.insertChar k
                    _ -> pure ()
    void $ grout (fixed $ pure $ CZ.lines c.input) $ row $ do
        grout (fixed $ pure $ Text.length inPrompt) $ text $ pure inPrompt
        void $
            grout flex $
                codeInput
                    def
                        { _codeInputConfig_value = Just $ pure c.input
                        , _codeInputConfig_showCursor = not c.disabled
                        }
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
    -- grout (fixed $ pure 1) $ row $ text $ current $ tshow <$> codeZipper
    pure cellEvent

redText :: forall t m. (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => Behavior t Text -> m ()
redText = richText RichTextConfig{_richTextConfig_attributes = pure $ V.withForeColor V.currentAttr V.red}
