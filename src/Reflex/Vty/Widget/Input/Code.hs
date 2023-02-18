{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Reflex.Vty.Widget.Input.Code where

import Control.Arrow ((>>>))
import Control.Lens.Operators
import Control.Monad.Fix (MonadFix)
import Data.Default (Default)
import Data.Either.Extra (fromRight, maybeToEither)
import Data.Generics.Labels ()
import Data.Generics.Product (position)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Zipper (DisplayLines (..), Span (..), TextAlignment (..), TextZipper, mapZipper)
import Data.Text.Zipper qualified as TZ
import Data.Tuple.Extra (both)
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Reflex hiding (tag)
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Input.Mouse
import Reflex.Vty.Widget.Input.Text
import Skylighting (TokenizerConfig (..), defaultSyntaxMap, lookupSyntax, tokenize)
import Skylighting.Types (SourceLine, Token, TokenType (..))

deriving instance Generic tag => Generic (Span tag)

data CodeZipper = CodeZipper
    { zipper :: TextZipper
    , language :: Text
    , highlighted :: [SourceLine]
    }
    deriving stock (Generic, Show)

codeZipperFromText :: Text -> Text -> CodeZipper
codeZipperFromText language code =
    highlightZipper
        CodeZipper
            { zipper = TZ.fromText code
            , language
            , highlighted = []
            }

data CodeInputConfig t = CodeInputConfig
    { _codeInputConfig_initialValue :: CodeZipper
    , _codeInputConfig_value :: Maybe (Dynamic t CodeZipper)
    , _codeInputConfig_modify :: Event t (CodeZipper -> CodeZipper)
    , _codeInputConfig_tabWidth :: Int
    , _codeInputConfig_display :: Dynamic t (Char -> Char)
    }

instance Reflex t => Default (CodeInputConfig t) where
    def =
        CodeInputConfig
            { _codeInputConfig_initialValue = codeZipperFromText "" ""
            , _codeInputConfig_value = Nothing
            , _codeInputConfig_modify = never
            , _codeInputConfig_tabWidth = 4
            , _codeInputConfig_display = pure id
            }

data CodeInput t = CodeInput
    { _codeInput_value :: Dynamic t Text
    , _codeInput_zipper :: Dynamic t CodeZipper
    , _codeInput_displayLines :: Dynamic t (DisplayLines V.Attr)
    , _codeInput_userInput :: Event t CodeZipper
    , _codeInput_lines :: Dynamic t Int
    }

codeInput
    :: forall t m
     . (Reflex t, MonadHold t m, MonadFix m, HasInput t m, HasFocusReader t m, HasTheme t m, HasDisplayRegion t m, HasImageWriter t m, HasDisplayRegion t m)
    => CodeInputConfig t
    -> m (CodeInput t)
codeInput cfg = mdo
    i <- input
    f <- focus
    dh <- displayHeight
    dw <- displayWidth
    bt <- theme
    attr0 <- sample bt
    let valueChangedByCaller, valueChangedByUI :: Event t (CodeZipper -> CodeZipper)
        valueChangedByCaller = _codeInputConfig_modify cfg
        valueChangedByUI =
            mergeWith
                (.)
                [ uncurry (updateCodeZipper (_codeInputConfig_tabWidth cfg)) <$> attach (current dh) i
                , let displayInfo = (,) <$> current rows <*> scrollTop
                   in ffor (attach displayInfo click) $ \((dl, st), MouseDown _ (mx, my) _) ->
                        updateZipper $ TZ.goToDisplayLinePosition mx (st + my) dl
                ]
    v :: Dynamic t CodeZipper <-
        flip fromMaybe (pure <$> cfg._codeInputConfig_value) $
            foldDyn ($) (_codeInputConfig_initialValue cfg) $
                mergeWith
                    (.)
                    [ valueChangedByCaller
                    , valueChangedByUI
                    ]
    click <- mouseDown V.BLeft

    -- TODO reverseVideo is prob not what we want. Does not work with `darkTheme` in example.hs (cursor is dark rather than light bg)
    let
        toCursorAttrs :: V.Attr -> V.Attr
        toCursorAttrs attr = V.withStyle attr V.reverseVideo
        rowInputDyn :: Dynamic t (Int, CodeZipper, Bool)
        rowInputDyn =
            (,,)
                <$> dw
                <*> (updateZipper . mapZipper <$> _codeInputConfig_display cfg <*> v)
                <*> f
        toDisplayLines :: V.Attr -> (Int, CodeZipper, Bool) -> DisplayLines V.Attr
        toDisplayLines attr (w, s, x) =
            let c = if x then toCursorAttrs attr else attr
             in displayCodeLines w attr c s
    attrDyn <- holdDyn attr0 $ pushAlways (\_ -> sample bt) (updated rowInputDyn)
    let rows = ffor2 attrDyn rowInputDyn toDisplayLines
        img = images . _displayLines_spans <$> rows
    y <- holdUniqDyn $ fmap snd _displayLines_cursorPos <$> rows
    let newScrollTop :: Int -> (Int, Int) -> Int
        newScrollTop st (h, cursorY)
            | cursorY < st = cursorY
            | cursorY >= st + h = cursorY - h + 1
            | otherwise = st
    let hy = attachWith newScrollTop scrollTop $ updated $ zipDyn dh y
    scrollTop <- hold 0 hy
    tellImages $ (\imgs st -> (: []) . V.vertCat $ drop st imgs) <$> current img <*> scrollTop
    return $
        CodeInput
            { _codeInput_value = TZ.value . zipper <$> v
            , _codeInput_zipper = v
            , _codeInput_displayLines = rows
            , _codeInput_userInput = attachWith (&) (current v) valueChangedByUI
            , _codeInput_lines = length . _displayLines_spans <$> rows
            }

{- | Given a width and a 'TextZipper', produce a list of display lines
 (i.e., lines of wrapped text) with special attributes applied to
 certain segments (e.g., the cursor). Additionally, produce the current
 y-coordinate of the cursor and a mapping from display line number to text
 offset
-}
displayCodeLinesWithAlignment
    :: TextAlignment
    -> Int
    -- ^ Width, used for wrapping
    -> V.Attr
    -- ^ Metadata for normal characters
    -> V.Attr
    -- ^ Metadata for the cursor
    -> CodeZipper
    -- ^ The text input contents and cursor state
    -> DisplayLines V.Attr
displayCodeLinesWithAlignment alignment width tag cursorTag z =
    DisplayLines
        { _displayLines_spans = lineToSpans <$> zip [0 ..] z.highlighted
        , _displayLines_cursorPos = (cursorCol, cursorRow)
        , _displayLines_offsetMap = mempty
        }
  where
    cursorRow = length z.zipper._textZipper_linesBefore
    cursorCol = Text.length z.zipper._textZipper_before
    tokenToSpan (tokenType, code) = highlightSpan tokenType $ Span tag code
    tokenToCursorSpan = cursorSpan . tokenToSpan
    lineToSpans (row, line)
        | row /= cursorRow = tokenToSpan <$> ((NormalTok, "") : line)
        | otherwise = do
            let (len, spans) = foldl appendTokenInCursorRow (0, []) line
            spans ++ [tokenToCursorSpan (NormalTok, " ") | len == cursorCol]
    tokenWidth = Text.length . snd
    appendTokenInCursorRow (rowWidth, rowSpans) t =
        let newWidth = tokenWidth t + rowWidth
            newSpans = rowSpans ++ tokenToSpansInCursorRow t rowWidth
         in (newWidth, newSpans)
    tokenToSpansInCursorRow t col
        | tokenWidth t == 0 = []
        | col <= cursorCol && (col + tokenWidth t) > cursorCol =
            let (a, b, c) = splitTokenAt' tokenCol t
                tokenCol = cursorCol - col
             in [tokenToSpan a, tokenToCursorSpan b, tokenToSpan c]
        | otherwise = [tokenToSpan t]

cursorSpan :: Span V.Attr -> Span V.Attr
cursorSpan = position @1 %~ flip V.withStyle V.reverseVideo

nonEmptyToken :: Token -> Token
nonEmptyToken (tokenType, tokenText) = (tokenType,) $ if Text.null tokenText then " " else tokenText

splitTokenAt :: Int -> Token -> (Token, Token)
splitTokenAt i (tokenType, tokenText) = both (tokenType,) $ Text.splitAt i tokenText

splitTokenAt' :: Int -> Token -> (Token, Token, Token)
splitTokenAt' i t = (a, b, c')
  where
    (a, c) = splitTokenAt i t
    (b, c') = splitTokenAt 1 c

highlightSpan :: TokenType -> Span V.Attr -> Span V.Attr
highlightSpan = maybe id (\c -> position @1 %~ flip V.withForeColor c) . tokenTypeColor

tokenTypeColor :: TokenType -> Maybe V.Color
tokenTypeColor CharTok = Just $ base08 @DefaultDark
tokenTypeColor KeywordTok = Just $ base0E @DefaultDark
tokenTypeColor DataTypeTok = Just $ base0A @DefaultDark
tokenTypeColor DecValTok = Just $ base09 @DefaultDark
tokenTypeColor BaseNTok = Just $ base09 @DefaultDark
tokenTypeColor FloatTok = Just $ base09 @DefaultDark
tokenTypeColor ConstantTok = Just $ base09 @DefaultDark
tokenTypeColor SpecialCharTok = Just $ base0F @DefaultDark
tokenTypeColor StringTok = Just $ base0B @DefaultDark
tokenTypeColor VerbatimStringTok = Just $ base0B @DefaultDark
tokenTypeColor SpecialStringTok = Just $ base0B @DefaultDark
tokenTypeColor ImportTok = Just $ base0D @DefaultDark
tokenTypeColor CommentTok = Just $ base03 @DefaultDark
tokenTypeColor DocumentationTok = Just $ base08 @DefaultDark
tokenTypeColor AnnotationTok = Just $ base0F @DefaultDark
tokenTypeColor CommentVarTok = Just $ base03 @DefaultDark
tokenTypeColor OtherTok = Just $ base05 @DefaultDark
tokenTypeColor FunctionTok = Just $ base0D @DefaultDark
tokenTypeColor VariableTok = Just $ base08 @DefaultDark
tokenTypeColor ControlFlowTok = Just $ base0E @DefaultDark
tokenTypeColor OperatorTok = Just $ base05 @DefaultDark
tokenTypeColor BuiltInTok = Just $ base0D @DefaultDark
tokenTypeColor ExtensionTok = Just $ base05 @DefaultDark
tokenTypeColor PreprocessorTok = Just $ base0A @DefaultDark
tokenTypeColor AttributeTok = Just $ base0A @DefaultDark
tokenTypeColor RegionMarkerTok = Just $ base05 @DefaultDark
tokenTypeColor InformationTok = Just $ base05 @DefaultDark
tokenTypeColor WarningTok = Just $ base08 @DefaultDark
tokenTypeColor AlertTok = Just $ base00 @DefaultDark
tokenTypeColor ErrorTok = Just $ base00 @DefaultDark
tokenTypeColor NormalTok = Just $ base05 @DefaultDark

class Base16 a where
    base00 :: V.Color
    base01 :: V.Color
    base02 :: V.Color
    base03 :: V.Color
    base04 :: V.Color
    base05 :: V.Color
    base06 :: V.Color
    base07 :: V.Color
    base08 :: V.Color
    base09 :: V.Color
    base0A :: V.Color
    base0B :: V.Color
    base0C :: V.Color
    base0D :: V.Color
    base0E :: V.Color
    base0F :: V.Color

data DefaultDark

instance Base16 DefaultDark where
    base00 = V.rgbColor @Int 0x18 0x18 0x18
    base01 = V.rgbColor @Int 0x28 0x28 0x28
    base02 = V.rgbColor @Int 0x38 0x38 0x38
    base03 = V.rgbColor @Int 0x58 0x58 0x58
    base04 = V.rgbColor @Int 0xB8 0xB8 0xB8
    base05 = V.rgbColor @Int 0xD8 0xD8 0xD8
    base06 = V.rgbColor @Int 0xE8 0xE8 0xE8
    base07 = V.rgbColor @Int 0xF8 0xF8 0xF8
    base08 = V.rgbColor @Int 0xAb 0x46 0x42
    base09 = V.rgbColor @Int 0xDc 0x96 0x56
    base0A = V.rgbColor @Int 0xF7 0xCa 0x88
    base0B = V.rgbColor @Int 0xA1 0xB5 0x6c
    base0C = V.rgbColor @Int 0x86 0xC1 0xB9
    base0D = V.rgbColor @Int 0x7c 0xAf 0xC2
    base0E = V.rgbColor @Int 0xBa 0x8b 0xAf
    base0F = V.rgbColor @Int 0xA1 0x69 0x46

{- | Given a width and a 'TextZipper', produce a list of display lines
 (i.e., lines of wrapped text) with special attributes applied to
 certain segments (e.g., the cursor). Additionally, produce the current
 y-coordinate of the cursor and a mapping from display line number to text
 offset
-}
displayCodeLines
    :: Int
    -- ^ Width, used for wrapping
    -> V.Attr
    -- ^ Metadata for normal characters
    -> V.Attr
    -- ^ Metadata for the cursor
    -> CodeZipper
    -- ^ The text input contents and cursor state
    -> DisplayLines V.Attr
displayCodeLines = displayCodeLinesWithAlignment TextAlignment_Left

updateZipper :: (TextZipper -> TextZipper) -> CodeZipper -> CodeZipper
updateZipper = (#zipper %~)

highlight :: Text -> Text -> [SourceLine]
highlight lang code = fromRight plain $ do
    syntax <- maybeToEither @String "Syntax not found" $ lookupSyntax lang defaultSyntaxMap
    let cfg = TokenizerConfig{syntaxMap = defaultSyntaxMap, traceOutput = False}
    case tokenize cfg syntax (code <> "\n") of
        Left _ -> Left "Tokenize failed"
        Right [] -> Left "No tokens produced"
        Right x -> Right x
  where
    plain = [(NormalTok,) <$> Text.lines (code <> "\n")]

highlightZipper :: CodeZipper -> CodeZipper
highlightZipper z = z & #highlighted .~ highlight z.language (TZ.value z.zipper)

-- | Default vty event handler for text inputs
updateCodeZipper
    :: Int
    -- ^ Tab width
    -> Int
    -- ^ Page size
    -> V.Event
    -- ^ The vty event to handle
    -> CodeZipper
    -- ^ The zipper to modify
    -> CodeZipper
updateCodeZipper tabWidth pageSize ev = case ev of
    -- Special characters
    V.EvKey (V.KChar '\t') [] -> updateZipper (TZ.tab tabWidth) >>> highlightZipper
    -- Regular characters
    V.EvKey (V.KChar k) [] -> updateZipper (TZ.insertChar k) >>> highlightZipper
    -- Deletion buttons
    V.EvKey V.KBS [] -> updateZipper TZ.deleteLeft >>> highlightZipper
    V.EvKey V.KDel [] -> updateZipper TZ.deleteRight >>> highlightZipper
    -- Arrow keys
    V.EvKey V.KLeft [] -> updateZipper TZ.left
    V.EvKey V.KRight [] -> updateZipper TZ.right
    V.EvKey V.KUp [] -> updateZipper TZ.up
    V.EvKey V.KDown [] -> updateZipper TZ.down
    V.EvKey V.KHome [] -> updateZipper TZ.home
    V.EvKey V.KEnd [] -> updateZipper TZ.end
    V.EvKey V.KPageUp [] -> updateZipper $ TZ.pageUp pageSize
    V.EvKey V.KPageDown [] -> updateZipper $ TZ.pageDown pageSize
    _ -> id
