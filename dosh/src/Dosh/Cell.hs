{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.Cell where

import Control.Monad.Fix
import Data.Default (Default)
import Data.Generics.Labels ()
import Data.List (sortBy)
import Data.List.Extra (groupOn)
import Data.Ord (comparing)
import Data.Text qualified as Text
import Data.Text.CodeZipper qualified as CZ
import Data.Text.Encoding qualified as Text
import Data.Text.Zipper (Span (Span))
import Data.Tuple.Extra (thd3)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Dosh.Prelude
import Dosh.Util
import Graphics.Vty qualified as V
import Language.Haskell.TH.Syntax (thenCmp)
import Language.LSP.Types (Diagnostic, DiagnosticSeverity (..))
import Language.LSP.Types.Lens (message, severity)
import Reflex
import Reflex.Vty
import Reflex.Vty.Widget.Input.Code
import Skylighting (TokenType)

type CodeZipper = CZ.CodeZipper TokenType

data Cell = Cell
    { uid :: UUID
    , number :: Int
    , firstLine :: Int
    , input :: CodeZipper
    , output :: Maybe ByteString
    , error :: Maybe Text
    , disabled :: Bool
    , evaluated :: Bool
    , diagnostics :: [Diagnostic]
    }
    deriving stock (Generic, Eq, Show)

newCell :: MonadIO m => (Cell -> Cell) -> m Cell
newCell f = liftIO UUID.nextRandom <&> \uid -> f def{uid}

instance Default Cell where
    def =
        Cell
            { uid = UUID.nil
            , number = 0
            , firstLine = 0
            , input = mempty
            , output = Nothing
            , error = Nothing
            , disabled = True
            , evaluated = False
            , diagnostics = []
            }

lastLine :: Cell -> Int
lastLine Cell{..} = firstLine + CZ.lines input

data CellEvent
    = UpdateCellInput InputUpdate
    | UpdateCellCursor CursorMove
    | EvaluateCell
    | GoToPreviousCell
    | GoToNextCell

data CursorMove
    = CursorUp Int
    | CursorDown Int
    | CursorLeft Int
    | CursorRight Int
    | CursorHome
    | CursorEnd
    | CursorTop
    | CursorBottom

moveCursor :: CursorMove -> (CodeZipper -> CodeZipper)
moveCursor (CursorUp n) = CZ.upN n
moveCursor (CursorDown n) = CZ.downN n
moveCursor (CursorLeft n) = CZ.leftN n
moveCursor (CursorRight n) = CZ.rightN n
moveCursor CursorHome = CZ.home
moveCursor CursorEnd = CZ.end
moveCursor CursorTop = CZ.top
moveCursor CursorBottom = CZ.bottom

data InputUpdate
    = DeleteLeft
    | DeleteRight
    | Insert Text

updateZipper :: InputUpdate -> (CodeZipper -> CodeZipper)
updateZipper DeleteLeft = CZ.deleteLeft
updateZipper DeleteRight = CZ.deleteRight
updateZipper (Insert t) = CZ.insert t

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
    let inPrompt = mconcat [if c.evaluated then "*" else " ", "In[", tshow c.number, "]: "]
        outPrompt = "Out[" <> tshow c.number <> "]: "
        errPrompt = "Err[" <> tshow c.number <> "]: "
        virtualLines :: [(Int, [Span V.Attr])]
        virtualLines = concatMap diagLines $ groupOn diagnosticLine $ sortBy diagOrd c.diagnostics
          where
            diagOrd :: Diagnostic -> Diagnostic -> Ordering
            diagOrd d1 d2 = comparing diagnosticLine d1 d2 `thenCmp` comparing diagnosticChar d1 d2
            diagLines :: [Diagnostic] -> [(Int, [Span V.Attr])]
            diagLines =
                thd3
                    . foldl'
                        ( \(padWidth, pads, lines) d ->
                            let c = diagnosticChar d
                                deltaC = c - padWidth
                                BoxStyle{..} = roundedBoxStyle
                                newPads = [Span V.currentAttr spaces | let spaces = Text.replicate deltaC " ", not (Text.null spaces)]
                                newPipe = [Span (diagAttr d) (Text.singleton _boxStyle_e) | deltaC > 0 || padWidth == 0]
                                newLines =
                                    Text.lines (prependAndPadLines (Text.pack [_boxStyle_sw, _boxStyle_s, ' ']) $ d ^. message) <&> \l ->
                                        (diagnosticLine d, pads <> newPads <> [Span (diagAttr d) l])
                             in ( c + 1
                                , pads <> newPads <> newPipe
                                , newLines <> lines
                                )
                        )
                        (0, [], [])
            diagAttr :: Diagnostic -> V.Attr
            diagAttr =
                view severity >>> \case
                    Just DsError -> V.withForeColor V.currentAttr V.red
                    Just DsWarning -> V.withForeColor V.currentAttr V.magenta
                    _ -> V.currentAttr
    (cellEvent, triggerCellEvent) <- newTriggerEvent
    unless c.disabled $ void $ do
        vtyInput :: Event t VtyEvent <- Reflex.Vty.input
        dh :: Dynamic t Int <- displayHeight
        let updateZipper = triggerCellEvent . UpdateCellInput
            updateCursor = triggerCellEvent . UpdateCellCursor
        performEvent $
            current dh `attach` vtyInput <&> \(dh, ev) ->
                liftIO $ case ev of
                    -- Delete character in zipper
                    V.EvKey V.KBS [] -> updateZipper DeleteLeft
                    V.EvKey V.KDel [] -> updateZipper DeleteRight
                    -- Movement in zipper and between cells
                    V.EvKey V.KUp [] ->
                        if null c.input.linesBefore
                            then triggerCellEvent GoToPreviousCell
                            else updateCursor $ CursorUp 1
                    V.EvKey V.KDown [] ->
                        if null c.input.linesAfter
                            then triggerCellEvent GoToNextCell
                            else updateCursor $ CursorDown 1
                    V.EvKey V.KLeft [] -> updateCursor $ CursorLeft 1
                    V.EvKey V.KRight [] -> updateCursor $ CursorRight 1
                    V.EvKey V.KHome [] -> updateCursor CursorHome
                    V.EvKey V.KEnd [] -> updateCursor CursorEnd
                    V.EvKey V.KPageUp [] -> updateCursor $ CursorUp dh
                    V.EvKey V.KPageDown [] -> updateCursor $ CursorDown dh
                    -- Insert characters into zipper
                    V.EvKey (V.KChar '\t') [] -> do
                        -- move to the next multiple of 4
                        let x = CZ.col c.input
                            dx = 4 - mod x 4
                        updateZipper $ Insert $ Text.replicate dx " "
                    V.EvKey (V.KChar k) [] -> updateZipper $ Insert $ Text.singleton k
                    V.EvKey V.KEnter [V.MMeta] -> updateZipper $ Insert $ Text.singleton '\n'
                    -- Evaluate the cell if it has any input
                    V.EvKey V.KEnter [] -> triggerCellEvent EvaluateCell
                    _ -> pure ()
    grout (fixed $ pure $ length virtualLines + CZ.lines c.input) $ row $ do
        grout (fixed $ pure $ Text.length inPrompt) $ text $ pure inPrompt
        -- let w = length (show $ lastLine c)
        -- grout (fixed $ pure $ w + 1) $ col $ forM_ [firstLine c .. lastLine c] $ \l ->
        --    let t = tshow l
        --        pad = w - Text.length t
        --        tp = Text.replicate pad " " <> t
        --     in grout (fixed $ pure 1) $ dimText $ pure tp
        grout flex $
            codeInput
                def
                    { _codeInputConfig_value = Just $ pure c.input
                    , _codeInputConfig_virtualLines = virtualLines
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
    pure cellEvent

colorText :: forall t m. (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => V.Color -> Behavior t Text -> m ()
colorText c = richText RichTextConfig{_richTextConfig_attributes = pure $ V.withForeColor V.currentAttr c}

dimText :: forall t m. (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => Behavior t Text -> m ()
dimText = richText RichTextConfig{_richTextConfig_attributes = pure $ V.withStyle V.currentAttr V.dim}
