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
    deriving stock (Show, Generic)

newCell :: MonadIO m => Int -> m Cell
newCell number = do
    uid <- liftIO UUID.nextRandom
    pure $ def{uid, number}

instance Default Cell where
    def =
        Cell
            { uid = UUID.nil
            , number = 0
            , input = CZ.fromText "Haskell" ""
            , output = Nothing
            , error = Nothing
            , disabled = True
            , evaluated = False
            }

data CellEvent
    = UpdateCellInput (CodeZipper TokenType)
    | EvaluateCell (CodeZipper TokenType)
    | GoToPreviousCell
    | GoToNextCell
    deriving stock (Show)

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
    (zipperEvent, updateZipper) <- newTriggerEvent @t @m @(CodeZipper TokenType -> CodeZipper TokenType)
    let inPrompt = mconcat [if c.evaluated then "*" else " ", "In[", tshow c.number, "]: "]
    let outPrompt = "Out[" <> tshow c.number <> "]: "
    let errPrompt = "Err[" <> tshow c.number <> "]: "
    codeZipper <- foldDyn ($) c.input zipperEvent
    unless c.disabled $ do
        vtyInput :: Event t VtyEvent <- Reflex.Vty.input
        dh :: Dynamic t Int <- displayHeight
        void $
            performEvent $
                (current codeZipper, current dh) `attach2` vtyInput <&> \(cz, dh, ev) -> liftIO $ case ev of
                    -- Delete character in zipper
                    V.EvKey V.KBS [] -> updateZipper CZ.deleteLeft
                    V.EvKey V.KDel [] -> updateZipper CZ.deleteRight
                    -- Movement in zipper and between cells
                    V.EvKey V.KUp [] -> if null cz.linesBefore then triggerCellEvent GoToPreviousCell else updateZipper CZ.up
                    V.EvKey V.KDown [] -> if null cz.linesAfter then triggerCellEvent GoToNextCell else updateZipper CZ.down
                    V.EvKey V.KLeft [] -> updateZipper CZ.left
                    V.EvKey V.KRight [] -> updateZipper CZ.right
                    V.EvKey V.KHome [] -> updateZipper CZ.home
                    V.EvKey V.KEnd [] -> updateZipper CZ.end
                    V.EvKey V.KPageUp [] -> updateZipper $ CZ.upN dh
                    V.EvKey V.KPageDown [] -> updateZipper $ CZ.downN dh
                    -- Insert characters into zipper
                    V.EvKey V.KEnter [] -> triggerCellEvent $ EvaluateCell cz
                    V.EvKey V.KEnter [V.MMeta] -> updateZipper $ CZ.insertChar '\n'
                    V.EvKey (V.KChar k) [] -> updateZipper $ CZ.insertChar k
                    _ -> pure ()
    void $ grout (fixed $ length . CZ.allLines <$> codeZipper) $ row $ do
        grout (fixed $ pure $ Text.length inPrompt) $ text $ pure inPrompt
        void $ grout flex $ codeInput
            def
                { _codeInputConfig_value = Just codeZipper
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
