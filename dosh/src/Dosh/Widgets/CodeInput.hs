{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Dosh.Widgets.CodeInput where

import Data.Foldable.Extra (sumOn')
import Data.Integral
import Data.Interval qualified as Interval
import Data.IntervalMap.Strict (IntervalMap)
import Data.IntervalMap.Strict qualified as IntervalMap
import Data.List qualified as List
import Data.Position
import Data.Text qualified as Text
import Data.Text.Rope qualified as Rope
import Data.Text.Rope.Extra qualified as Rope
import Data.Text.Rope.Zipper qualified as RopeZipper
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Prettyprinter qualified
import System.Terminal qualified as Terminal
import System.Terminal.Widget
import System.Terminal.Widget qualified as Widget
import Prelude
import Data.Generics.Product qualified as Lens
import Data.Text.Rope.Zipper (RopeZipper)
import Prettyprinter (defaultLayoutOptions, layoutPretty)

data CodeInput = CodeInput
    { input :: TextInput
    , tokens :: IntervalMap LSP.Position LSP.SemanticTokenTypes
    , diagnostics :: [LSP.Diagnostic]
    , lastChange :: UTCTime
    }
    deriving stock (Generic)

instance {-# OVERLAPPING #-} Lens.HasField "value" CodeInput CodeInput RopeZipper RopeZipper where
    field = #input . #value

instance {-# OVERLAPPING #-} Lens.HasField "cursor" CodeInput CodeInput Terminal.Position Terminal.Position where
    field = #input . Widget.cursor

withLastChange :: UTCTime -> Lens' CodeInput CodeInput
withLastChange time = lens id $ const $ #lastChange .~ time

withVirtualLines :: forall p. (Position p) => [LSP.Diagnostic] -> Iso' p p
withVirtualLines diagnostics = iso sa bt
  where
    virtualLinesUpTo line =
        sumOn' (length . Text.lines . (._message))
            . filter (\d -> d._range._start._line < fromIntegral line)
            $ diagnostics
    sa :: p -> p
    sa = row %~ \r -> r + virtualLinesUpTo r
    bt :: p -> p
    bt = row %~ \r -> r - virtualLinesUpTo r

instance Widget CodeInput where
    cursor = lens getter setter
      where
        getter :: CodeInput -> Terminal.Position
        getter CodeInput{..} = input ^. cursor . withVirtualLines diagnostics
        setter :: CodeInput -> Terminal.Position -> CodeInput
        setter input pos = input & #input . cursor . withVirtualLines input.diagnostics .~ pos
    handleEvent = over #input . handleEvent
    submitEvent = submitEvent . (.input)
    valid = valid . (.input)
    lineCount CodeInput{..} = lineCount input + sumOn' (length . Text.lines . (._message)) diagnostics
    toDocStream CodeInput{..} = layoutPretty defaultLayoutOptions $ go (Rope.Position 0 0) rope intervals diagnostics
      where
        rope = padRopeLines input.prompt $ RopeZipper.toRope input.value
        ropeStart = Rope.Position 0 0
        ropeEnd = Rope.lengthAsPosition rope
        lspPosToRopePos =
            clampExtended (ropeStart, ropeEnd)
                . fmap (LSP.character . integral +~ Text.length input.prompt >>> view lspRopePos)
        intervals = IntervalMap.toAscList tokens
        go
            :: (MonadColorPrinter m)
            => Rope.Position
            -> Rope
            -> [(Interval LSP.Position, LSP.SemanticTokenTypes)]
            -> [LSP.Diagnostic]
            -> Doc (Attribute m)
        go pos r intervals diagnostics
            | ((interval, tokenType) : otherIntervals) <- intervals
            , (_, relA) <- ropePos $ Interval.lowerBound interval
            , relA.posLine == 0
            , (b, relB) <- ropePos $ Interval.upperBound interval
            , (prefix, token, suffix) <- sliceRope (relA, relB) currentLine =
                mconcat
                    [ pretty prefix
                    , if Rope.null token
                        then mempty
                        else tokenToDoc tokenType . pretty $ token
                    , go b (suffix <> otherLines) otherIntervals diagnostics
                    ]
            | otherwise =
                mconcat
                    [ pretty currentLine
                    , if not (null currentLineDiagnostics)
                        && Rope.null otherLines
                        && not (Rope.hasTrailingNewline currentLine)
                        then Prettyprinter.line
                        else mempty
                    , renderDiagnostics promptPad currentLineDiagnostics
                    , if Rope.null otherLines
                        then mempty
                        else go (pos & row +~ 1 & col .~ 0) otherLines intervals otherLineDiagnostics
                    ]
          where
            (currentLine, otherLines) = Rope.splitAtLine 1 r
            ropePos lspPos = let x = lspPosToRopePos lspPos in (x, x ^. relativePos pos)
            (currentLineDiagnostics, otherLineDiagnostics) =
                List.span (\d -> d._range._start._line == fromIntegral pos.posLine) diagnostics
            promptPad = pretty $ Text.replicate (Text.length input.prompt) " "
    render
        :: (MonadTerminal m)
        => Maybe CodeInput
        -> CodeInput
        -> m ()
    render maybeOld new = when shouldRender do
        defaultRender maybeOld new
        Terminal.flush
      where
        shouldRender = maybe True (\old -> new.lastChange > old.lastChange) maybeOld

ropeHasTrailingNewline :: Rope -> Bool
ropeHasTrailingNewline (Rope.toText -> Text.uncons -> Just ('\n', _)) = True
ropeHasTrailingNewline _ = False

renderDiagnostics
    :: forall m
     . (MonadColorPrinter m)
    => Doc (Attribute m)
    -> [LSP.Diagnostic]
    -> Doc (Attribute m)
renderDiagnostics _ [] = mempty
renderDiagnostics pad (d : ds) = mconcat messageLines <> renderDiagnostics pad ds
  where
    messageLines =
        zipWith (\i t -> startColPad i <> colour d (t <> "\n")) [0 :: Int ..] $
            Text.lines d._message
    lastLen = maybe 0 (fromIntegral . (+ 3) . (._range._start._character)) (listToMaybe ds)
    startColPad line =
        fst $
            foldr
                ( \pd (pad, len) ->
                    let dl = fromIntegral pd._range._start._character - len
                        ch
                            | line == 0 && len == lastLen = "└─ "
                            | len == lastLen = "   "
                            | otherwise = "│  "
                     in ( pad <> pretty (Text.replicate dl " ") <> colour pd ch
                        , fromIntegral pd._range._start._character + 3
                        )
                )
                (pad, 0)
                (d : ds)
    colour :: LSP.Diagnostic -> Text -> Doc (Attribute m)
    colour =
        (._severity) >>> \case
            Nothing -> pretty
            Just LSP.DiagnosticSeverity_Error -> annotate (foreground red) . pretty
            Just LSP.DiagnosticSeverity_Warning -> annotate (foreground magenta) . pretty
            Just LSP.DiagnosticSeverity_Information -> annotate (foreground yellow) . pretty
            Just LSP.DiagnosticSeverity_Hint -> annotate (foreground cyan) . pretty

lspRopePos :: Iso' LSP.Position Rope.Position
lspRopePos = iso sa bt
  where
    sa :: LSP.Position -> Rope.Position
    sa LSP.Position{..} =
        Rope.Position{posLine = fromIntegral _line, posColumn = fromIntegral _character}
    bt :: Rope.Position -> LSP.Position
    bt Rope.Position{..} =
        LSP.Position{_line = fromIntegral posLine, _character = fromIntegral posColumn}

tokenToDoc
    :: forall m
     . (MonadColorPrinter m)
    => LSP.SemanticTokenTypes
    -> Doc (Attribute m)
    -> Doc (Attribute m)
tokenToDoc t =
    maybe id (annotate . foreground . snd) . find ((t ==) . fst) $
        zip knownTypes colours
  where
    knownTypes :: [LSP.SemanticTokenTypes]
    knownTypes = toList LSP.knownValues
    colours :: [Color m]
    colours = cycle [red, green, yellow, blue, magenta, cyan]

relativePos :: Rope.Position -> Iso' Rope.Position Rope.Position
relativePos a = iso sa bt
  where
    sa b =
        if a.posLine == b.posLine
            then Rope.Position{posLine = 0, posColumn = b.posColumn - a.posColumn}
            else Rope.Position{posLine = b.posLine - a.posLine, posColumn = b.posColumn}
    bt d =
        if d.posLine == 0
            then Rope.Position{posLine = a.posLine, posColumn = a.posColumn + d.posColumn}
            else Rope.Position{posLine = a.posLine + d.posLine, posColumn = d.posColumn}

-- | Slice the rope between the given indices.
sliceRope :: (Rope.Position, Rope.Position) -> Rope -> (Rope, Rope, Rope)
sliceRope (a, b) _ | a > b = error $ "sliceRope: interval start after end: " <> show (a, b)
sliceRope (a, b) r
    | numLines > 1 =
        let (firstLine, (middleLines, lastLine)) =
                focusLines
                    & Rope.splitAtLine 1
                    & second (Rope.splitAtLine $ max 1 $ numLines - 2)
            (prefix, firstLineFocus) = firstLine & Rope.splitAt a.posColumn
            (lastLineFocus, suffix) = lastLine & Rope.splitAt b.posColumn
         in ( linesBefore <> prefix
            , firstLineFocus <> middleLines <> lastLineFocus
            , suffix <> linesAfter
            )
    | otherwise =
        let (prefix, (focus, suffix)) = focusLines & Rope.splitAt a.posColumn & second (Rope.splitAt relB.posColumn)
         in (linesBefore <> prefix, focus, suffix <> linesAfter)
  where
    relB = b ^. relativePos a
    numLines = relB.posLine + 1
    linesBefore, focusLines, linesAfter :: Rope
    (linesBefore, (focusLines, linesAfter)) = r & Rope.splitAtLine a.posLine & second (Rope.splitAtLine numLines)

-- | Prepend the prefix to every line of the rope.
-- The first line gets the content of the prefix, other lines get padded with spaces to match the width of the prefix.
padRopeLines :: Text -> Rope -> Rope
padRopeLines prefix r =
    let pad = Rope.fromText $ Text.replicate (Text.length prefix) " "
        allLines = ropeLines r
     in case allLines of
            [] -> Rope.fromText prefix
            (firstLine : otherLines) -> Rope.fromText prefix <> firstLine <> mconcat (mappend pad <$> otherLines)
