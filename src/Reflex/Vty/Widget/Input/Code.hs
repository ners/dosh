{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Reflex.Vty.Widget.Input.Code where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Data.Default (Default)
import Data.Either.Extra (fromRight, maybeToEither)
import Data.Generics.Labels ()
import Data.Generics.Product (position)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
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
import Skylighting (Color (RGB), Style (..), ToColor (toColor), TokenStyle (..), TokenizerConfig (..), defStyle, defaultSyntaxMap, lookupSyntax, tokenize)
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
            , _codeInput_lines = CZ.lines <$> v
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
tokenAttr t =
    foldr (.) id $
        catMaybes
            [ fmap (flip V.withForeColor . vColor) $ (tokenStyle >>= tokenColor) <|> Skylighting.defaultColor style
            , fmap (flip V.withBackColor . vColor) $ tokenStyle >>= tokenBackground
            , fmap (bool id (`V.withStyle` V.bold)) $ tokenBold <$> tokenStyle
            , fmap (bool id (`V.withStyle` V.italic)) $ tokenItalic <$> tokenStyle
            , fmap (bool id (`V.withStyle` V.underline)) $ tokenUnderline <$> tokenStyle
            ]
  where
    style = base16DefaultDark
    tokenStyle = style.tokenStyles ^. at t
    vColor (RGB r g b) = V.rgbColor r g b

base16DefaultDark :: Style
base16DefaultDark =
    Style
        { backgroundColor = Nothing
        , defaultColor = base05
        , lineNumberColor = Nothing
        , lineNumberBackgroundColor = Nothing
        , tokenStyles =
            Map.fromList
                [ (KeywordTok, defStyle{tokenColor = base0E})
                , (DataTypeTok, defStyle{tokenColor = base0A, tokenUnderline = True})
                , (DecValTok, defStyle{tokenColor = base09})
                , (BaseNTok, defStyle{tokenColor = base09})
                , (FloatTok, defStyle{tokenColor = base09})
                , (CharTok, defStyle{tokenColor = base08})
                , (StringTok, defStyle{tokenColor = base0B})
                , (CommentTok, defStyle{tokenColor = base03, tokenItalic = True})
                , (AlertTok, defStyle{tokenColor = base00})
                , (FunctionTok, defStyle{tokenColor = base0D})
                , (ErrorTok, defStyle{tokenColor = base08, tokenBold = True})
                , (WarningTok, defStyle{tokenColor = base08, tokenBold = True})
                , (ConstantTok, defStyle{tokenColor = base09})
                , (SpecialCharTok, defStyle{tokenColor = base0B})
                , (VerbatimStringTok, defStyle{tokenColor = base0B})
                , (SpecialStringTok, defStyle{tokenColor = base0B})
                , (ImportTok, defStyle{tokenColor = base0D})
                , (VariableTok, defStyle{tokenColor = base08})
                , (ControlFlowTok, defStyle{tokenColor = base0E})
                , (OperatorTok, defStyle{tokenColor=base05})
                , (BuiltInTok, defStyle{tokenColor = base0D})
                , (ExtensionTok, defStyle{tokenColor = base05})
                , (PreprocessorTok, defStyle{tokenColor = base0A})
                , (AttributeTok, defStyle{tokenColor = base0A})
                , (DocumentationTok, defStyle{tokenColor = base08, tokenItalic = True})
                , (AnnotationTok, defStyle{tokenColor = base0F, tokenItalic = True})
                , (CommentTok, defStyle{tokenColor = base03, tokenItalic = True})
                , (InformationTok, defStyle{tokenColor = base03, tokenItalic = True})
                ]
        }
    where
        base00 = toColor @Int 0x181818
        base01 = toColor @Int 0x282828
        base02 = toColor @Int 0x383838
        base03 = toColor @Int 0x585858
        base04 = toColor @Int 0xb8b8b8
        base05 = toColor @Int 0xd8d8d8
        base06 = toColor @Int 0xe8e8e8
        base07 = toColor @Int 0xf8f8f8
        base08 = toColor @Int 0xab4642
        base09 = toColor @Int 0xdc9656
        base0A = toColor @Int 0xf7ca88
        base0B = toColor @Int 0xa1b56c
        base0C = toColor @Int 0x86c1b9
        base0D = toColor @Int 0x7cafc2
        base0E = toColor @Int 0xba8baf
        base0F = toColor @Int 0xa16946

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
