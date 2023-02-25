{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.Cell where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Default (Default)
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Data.Text.CodeZipper qualified as CZ
import Data.Text.Encoding qualified as Text
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Dosh.Prelude
import Dosh.Util
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Reflex
import Reflex.Vty
import Reflex.Vty.Widget.Input.Code
import Skylighting (TokenType)

type CodeZipper = CZ.CodeZipper TokenType

data Cell = Cell
    { uid :: UUID
    , number :: Int
    , input :: CodeZipper
    , output :: Maybe ByteString
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
    = UpdateCellInput (CodeZipper -> CodeZipper)
    | EvaluateCell Text
    | GoToPreviousCell
    | GoToNextCell

cell
    :: forall t m
     . ( PerformEvent t m
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
       )
    => Cell
    -> m (Event t CellEvent)
cell c = do
    (cellEvent, triggerCellEvent) <- newTriggerEvent @t @m @CellEvent
    let inPrompt = mconcat [if c.evaluated then "*" else " ", "In[", tshow c.number, "]: "]
    let outPrompt = "Out[" <> tshow c.number <> "]: "
    let errPrompt = "Err[" <> tshow c.number <> "]: "
    unless c.disabled $ void $ do
        vtyInput :: Event t VtyEvent <- Reflex.Vty.input
        dh :: Dynamic t Int <- displayHeight
        let updateZipper = triggerCellEvent . UpdateCellInput
        performEvent $
            current dh `attach` vtyInput <&> \(dh, ev) ->
                liftIO $ case ev of
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
                    V.EvKey (V.KChar '\t') [] -> do
                        -- move to the next multiple of 4
                        let x = CZ.col c.input
                            dx = 4 - mod x 4
                        updateZipper $ CZ.insert $ Text.replicate dx " "
                    V.EvKey (V.KChar k) [] -> updateZipper $ CZ.insertChar k
                    V.EvKey V.KEnter [V.MMeta] -> updateZipper $ CZ.insertChar '\n'
                    -- Evaluate the cell if it has any input
                    V.EvKey V.KEnter [] ->
                        unless (CZ.null $ c.input) $
                            triggerCellEvent $
                                EvaluateCell $
                                    CZ.toText c.input
                    _ -> pure ()
    grout (fixed $ pure $ CZ.lines c.input) $ row $ do
        grout (fixed $ pure $ Text.length inPrompt) $ text $ pure inPrompt
        grout flex $
            codeInput
                def
                    { _codeInputConfig_value = Just $ pure c.input
                    , _codeInputConfig_showCursor = not c.disabled
                    }
    forM_ c.output $ \out -> do
        blankLine
        let (height, content) = case Text.decodeUtf8' out of
                Right utf8 -> (length $ Text.lines utf8, text $ pure utf8)
                Left _ -> (10, display $ pure out)
        grout (fixed $ pure height) $ row $ do
            grout (fixed $ pure $ Text.length outPrompt) $ text $ pure outPrompt
            grout flex content
    forM_ c.error $ \err -> do
        blankLine
        grout (fixed $ pure $ length $ Text.lines err) $ row $ do
            grout (fixed $ pure $ Text.length errPrompt) $ text $ pure errPrompt
            grout flex $ colorText V.red $ pure err
    blankLine
    -- grout (fixed $ pure 1) $ row $ text $ current $ tshow <$> codeZipper
    pure cellEvent

colorText :: forall t m. (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => V.Color -> Behavior t Text -> m ()
colorText c = richText RichTextConfig{_richTextConfig_attributes = pure $ V.withForeColor V.currentAttr c}
