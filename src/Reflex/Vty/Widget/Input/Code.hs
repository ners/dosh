{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Reflex.Vty.Widget.Input.Code where

import Control.Lens.Operators
import Control.Monad.Fix (MonadFix)
import Data.Default (Default)
import Data.Either.Extra (fromRight, maybeToEither)
import Data.Generics.Labels ()
import Data.Generics.Product (position)
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.CodeZipper (CodeZipper)
import Data.Text.CodeZipper qualified as CZ
import Data.Text.Zipper (DisplayLines (..), Span (..), TextAlignment (..))
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Reflex hiding (tag)
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Input.Mouse
import Reflex.Vty.Widget.Input.Text
import Skylighting (TokenizerConfig (..), defaultSyntaxMap, lookupSyntax, tokenize)
import Skylighting.Types (SourceLine, Token, TokenType (..))

deriving instance Generic tag => Generic (Span tag)

instance CZ.Pretty TokenType where prettify = highlight

data CodeInputConfig t = CodeInputConfig
    { _codeInputConfig_initialValue :: CodeZipper TokenType
    , _codeInputConfig_value :: Maybe (Dynamic t (CodeZipper TokenType))
    , _codeInputConfig_modify :: Event t (CodeZipper TokenType -> CodeZipper TokenType)
    , _codeInputConfig_tabWidth :: Int
    , _codeInputConfig_display :: Dynamic t (Char -> Char)
    }

instance Reflex t => Default (CodeInputConfig t) where
    def =
        CodeInputConfig
            { _codeInputConfig_initialValue = CZ.fromText "" ""
            , _codeInputConfig_value = Nothing
            , _codeInputConfig_modify = never
            , _codeInputConfig_tabWidth = 4
            , _codeInputConfig_display = pure id
            }

data CodeInput t = CodeInput
    { _codeInput_value :: Dynamic t Text
    , _codeInput_zipper :: Dynamic t (CodeZipper TokenType)
    , _codeInput_displayLines :: Dynamic t (DisplayLines V.Attr)
    , _codeInput_userInput :: Event t (CodeZipper TokenType)
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
    let valueChangedByCaller, valueChangedByUI :: Event t (CodeZipper TokenType -> CodeZipper TokenType)
        valueChangedByCaller = _codeInputConfig_modify cfg
        valueChangedByUI =
            mergeWith
                (.)
                [ uncurry (updateCodeZipper (_codeInputConfig_tabWidth cfg)) <$> attach (current dh) i
                ]
    v :: Dynamic t (CodeZipper TokenType) <-
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
        rowInputDyn :: Dynamic t (Int, CodeZipper TokenType, Bool)
        rowInputDyn = (,,) <$> dw <*> v <*> f
        toDisplayLines :: V.Attr -> (Int, CodeZipper TokenType, Bool) -> DisplayLines V.Attr
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
            { _codeInput_value = CZ.toText <$> v
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
    -> CodeZipper TokenType
    -- ^ The text input contents and cursor state
    -> DisplayLines V.Attr
displayCodeLinesWithAlignment alignment width tag cursorTag z =
    DisplayLines
        { _displayLines_spans =
            mconcat
                [ lineToSpans <$> reverse z.linesBefore
                , [currentLineSpans]
                , lineToSpans <$> z.linesAfter
                ]
        , _displayLines_cursorPos = (cursorCol, cursorRow)
        , _displayLines_offsetMap = mempty
        }
  where
    cursorRow = length z.linesBefore
    cursorCol = CZ.lineWidth z.tokensBefore
    tokenToSpan (tokenType, code) = highlightSpan tokenType $ Span tag code
    cursorTokenToSpan = cursorSpan . tokenToSpan
    lineToSpans line = tokenToSpan <$> ((NormalTok, "") : line)
    (cursorToken, tokensAfter) = fromMaybe ((NormalTok, " "), []) $ do
        (ta, tas) <- uncons z.tokensAfter
        let (ct, ta') = CZ.splitTokenAt 1 ta
        pure (ct, ta' : tas)
    currentLineSpans =
        mconcat
            [ lineToSpans $ reverse z.tokensBefore
            , [cursorTokenToSpan cursorToken]
            , lineToSpans tokensAfter
            ]

cursorSpan :: Span V.Attr -> Span V.Attr
cursorSpan = position @1 %~ flip V.withStyle V.reverseVideo

nonEmptyToken :: Token -> Token
nonEmptyToken (tokenType, tokenText) = (tokenType,) $ if Text.null tokenText then " " else tokenText

highlightSpan :: TokenType -> Span V.Attr -> Span V.Attr
highlightSpan = (position @1 %~) . tokenAttr

tokenAttr :: TokenType -> V.Attr -> V.Attr
tokenAttr CharTok = flip V.withForeColor $ base08 @DefaultDark
tokenAttr KeywordTok = flip V.withForeColor $ base0E @DefaultDark
tokenAttr DataTypeTok = flip V.withForeColor $ base0A @DefaultDark
tokenAttr DecValTok = flip V.withForeColor $ base09 @DefaultDark
tokenAttr BaseNTok = flip V.withForeColor $ base09 @DefaultDark
tokenAttr FloatTok = flip V.withForeColor $ base09 @DefaultDark
tokenAttr ConstantTok = flip V.withForeColor $ base09 @DefaultDark
tokenAttr SpecialCharTok = flip V.withForeColor $ base0F @DefaultDark
tokenAttr StringTok = flip V.withForeColor $ base0B @DefaultDark
tokenAttr VerbatimStringTok = flip V.withForeColor $ base0B @DefaultDark
tokenAttr SpecialStringTok = flip V.withForeColor $ base0B @DefaultDark
tokenAttr ImportTok = flip V.withForeColor $ base0D @DefaultDark
tokenAttr CommentTok = flip V.withForeColor $ base03 @DefaultDark
tokenAttr DocumentationTok = flip V.withForeColor $ base08 @DefaultDark
tokenAttr AnnotationTok = flip V.withForeColor $ base0F @DefaultDark
tokenAttr CommentVarTok = flip V.withForeColor $ base03 @DefaultDark
tokenAttr OtherTok = flip V.withForeColor $ base0A @DefaultDark
tokenAttr FunctionTok = flip V.withForeColor $ base0D @DefaultDark
tokenAttr VariableTok = flip V.withForeColor $ base08 @DefaultDark
tokenAttr ControlFlowTok = flip V.withForeColor $ base0E @DefaultDark
tokenAttr OperatorTok = flip V.withForeColor $ base05 @DefaultDark
tokenAttr BuiltInTok = flip V.withForeColor $ base0D @DefaultDark
tokenAttr ExtensionTok = flip V.withForeColor $ base05 @DefaultDark
tokenAttr PreprocessorTok = flip V.withForeColor $ base0A @DefaultDark
tokenAttr AttributeTok = flip V.withForeColor $ base0A @DefaultDark
tokenAttr RegionMarkerTok = flip V.withForeColor $ base05 @DefaultDark
tokenAttr InformationTok = flip V.withForeColor $ base05 @DefaultDark
tokenAttr WarningTok = flip V.withForeColor $ base08 @DefaultDark
tokenAttr AlertTok = flip V.withForeColor $ base00 @DefaultDark
tokenAttr ErrorTok = flip V.withForeColor $ base00 @DefaultDark
tokenAttr NormalTok = flip V.withForeColor $ base05 @DefaultDark

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
    base08 = V.rgbColor @Int 0xAB 0x46 0x42
    base09 = V.rgbColor @Int 0xDC 0x96 0x56
    base0A = V.rgbColor @Int 0xF7 0xCA 0x88
    base0B = V.rgbColor @Int 0xA1 0xB5 0x6C
    base0C = V.rgbColor @Int 0x86 0xC1 0xB9
    base0D = V.rgbColor @Int 0x7C 0xAF 0xC2
    base0E = V.rgbColor @Int 0xBA 0x8B 0xAF
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
    -> CodeZipper TokenType
    -- ^ The text input contents and cursor state
    -> DisplayLines V.Attr
displayCodeLines = displayCodeLinesWithAlignment TextAlignment_Left

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

-- | Default vty event handler for text inputs
updateCodeZipper
    :: Int
    -- ^ Tab width
    -> Int
    -- ^ Page size
    -> V.Event
    -- ^ The vty event to handle
    -> CodeZipper TokenType
    -- ^ The zipper to modify
    -> CodeZipper TokenType
updateCodeZipper tabWidth pageSize ev = case ev of
    -- Special characters
    V.EvKey (V.KChar '\t') [] -> CZ.insertChar '\t'
    -- Regular characters
    V.EvKey (V.KChar k) [] -> CZ.insertChar k
    -- Deletion buttons
    V.EvKey V.KBS [] -> CZ.deleteLeft
    V.EvKey V.KDel [] -> CZ.deleteRight
    -- Arrow keys
    V.EvKey V.KLeft [] -> CZ.left
    V.EvKey V.KRight [] -> CZ.right
    V.EvKey V.KUp [] -> CZ.up
    V.EvKey V.KDown [] -> CZ.down
    V.EvKey V.KHome [] -> CZ.home
    V.EvKey V.KEnd [] -> CZ.end
    V.EvKey V.KPageUp [] -> CZ.upN pageSize
    V.EvKey V.KPageDown [] -> CZ.downN pageSize
    _ -> id
