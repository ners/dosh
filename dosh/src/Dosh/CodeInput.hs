module Dosh.CodeInput where

import Data.ExtendedReal qualified as Extended
import Data.Interval qualified as Interval
import Data.IntervalMap.Strict (IntervalMap)
import Data.IntervalMap.Strict qualified as IntervalMap
import Data.Text qualified as Text
import Data.Text.Rope qualified as Rope
import Data.Text.Rope.Zipper qualified as RopeZipper
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import System.Terminal
import System.Terminal.Widgets.Common
import System.Terminal.Widgets.TextInput
import Prelude

data CodeInput ann = CodeInput
    { input :: TextInput
    , tokens :: IntervalMap LSP.Position LSP.SemanticTokenTypes
    }
    deriving stock (Generic)

instance Widget (CodeInput ann) where
    cursor = #input . cursor
    handleEvent e = #input %~ handleEvent e
    submitEvent = submitEvent . (.input)
    valid = valid . (.input)
    lineCount = lineCount . (.input)
    toDoc CodeInput{..} = go (Rope.Position 0 0) paddedRope intervals
      where
        rope = RopeZipper.toRope input.value
        paddedRope = padRopeLines input.prompt rope
        ropeStart = Rope.Position 0 0
        ropeEnd = Rope.lengthAsPosition rope
        lspPosToRopePos =
            clampExtended (ropeStart, ropeEnd)
                . fmap (LSP.character . integral +~ Text.length input.prompt >>> view lspRopePos)
        intervals = IntervalMap.toAscList tokens
        go _ (Rope.null -> True) _ = ""
        go _ r [] = pretty r
        go pos r ((interval, tokenType) : rest) =
            let
                a = lspPosToRopePos $ Interval.lowerBound interval
                relA = a ^. relativePos pos
                b = lspPosToRopePos $ Interval.upperBound interval
                relB = b ^. relativePos pos
                (prefix, token, suffix) = sliceRope (relA, relB) r
             in
                mconcat
                    [ pretty prefix
                    , if Rope.null token
                        then mempty
                        else tokenToDoc tokenType . pretty $ token
                    , go b suffix rest
                    ]

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
    :: (MonadColorPrinter m)
    => LSP.SemanticTokenTypes
    -> Doc (Attribute m)
    -> Doc (Attribute m)
tokenToDoc LSP.SemanticTokenTypes_Type = annotate $ foreground red
tokenToDoc _ = id

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
            (firstLine : otherLines) -> Rope.fromText prefix <> firstLine <> mconcat ((pad <>) <$> otherLines)

codeInput :: IO Text
codeInput = do
    traceShowM tokens
    ci <-
        withTerminal . runTerminalT . runWidget $
            CodeInput
                { input =
                    TextInput
                        { prompt = "-> "
                        , multiline = True
                        , required = True
                        , value = ""
                        , valueTransform = id
                        }
                , tokens
                }
    pure $ RopeZipper.toText ci.input.value
  where
    positionToBound :: LSP.Position -> (Extended LSP.Position, Interval.Boundary)
    positionToBound pos = (Extended.Finite pos, Interval.Closed)
    tokens =
        IntervalMap.fromList
            [
                ( Interval.interval
                    (positionToBound $ LSP.Position 0 5)
                    (positionToBound $ LSP.Position 0 8)
                , LSP.SemanticTokenTypes_Type
                )
            ]
