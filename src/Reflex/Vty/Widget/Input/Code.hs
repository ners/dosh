{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Reflex.Vty.Widget.Input.Code where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad.Fix (MonadFix)
import Data.Default (Default)
import Data.Either.Extra (eitherToMaybe, maybeToEither)
import Data.Generics.Labels ()
import Data.Generics.Product (position)
import Data.List (intersperse)
import Data.List.Extra qualified
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.CodeZipper (CodeZipper (..), Pretty (..), normaliseToks, tokenLines)
import Data.Text.CodeZipper qualified as CZ
import Data.Text.Zipper (DisplayLines (..), Span (..), TextAlignment (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.SyntaxHighlighter qualified as GS
import Graphics.Vty qualified as V
import Reflex hiding (tag)
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Input.Mouse
import Reflex.Vty.Widget.Input.Text
import Skylighting qualified as S
import Skylighting.Types (TokenType (..))

deriving instance Generic tag => Generic (Span tag)

type Token = CZ.Token TokenType

type SourceLine = CZ.SourceLine TokenType

instance Pretty TokenType where
    plain code = [[CZ.Token{tokenType = NormalTok, ..}] | tokenContent <- Text.splitOn "\n" code]
    pretty "Haskell" = ghcHighlight
    pretty lang = skylight lang

plainToken :: Text -> Token
plainToken tokenContent = CZ.Token{tokenType = NormalTok, ..}

skylight :: Text -> Text -> Maybe [SourceLine]
skylight lang code = eitherToMaybe $ do
    syntax <- maybeToEither @String "Syntax not found" $ S.lookupSyntax lang S.defaultSyntaxMap
    let cfg = S.TokenizerConfig{syntaxMap = S.defaultSyntaxMap, traceOutput = False}
    case S.tokenize cfg syntax (code <> "\n") of
        Left _ -> Left "Tokenize failed"
        Right [] -> Left "No tokens produced"
        Right lines -> Right $ (fmap . fmap) (\(tokenType, tokenContent) -> CZ.Token{..}) lines

ghcHighlight :: Text -> Maybe [SourceLine]
ghcHighlight code = fmap normaliseToks . gsTokensToLines <$> GS.tokenizeHaskell code
  where
    gsTokensToLines :: [(GS.Token, Text)] -> [SourceLine]
    gsTokensToLines = Data.List.Extra.split (("\n" ==) . CZ.tokenContent) . concatMap (intersperse (plainToken "\n") . tokenLines . mapToken)
    mapToken :: (GS.Token, Text) -> Token
    mapToken (mapTokenType -> tokenType, tokenContent) = CZ.Token{..}
    mapTokenType :: GS.Token -> TokenType
    mapTokenType GS.CharTok = CharTok
    mapTokenType GS.CommentTok = CommentTok
    mapTokenType GS.ConstructorTok = FunctionTok
    mapTokenType GS.IntegerTok = ConstantTok
    mapTokenType GS.KeywordTok = KeywordTok
    mapTokenType GS.OperatorTok = OperatorTok
    mapTokenType GS.OtherTok = OtherTok
    mapTokenType GS.PragmaTok = PreprocessorTok
    mapTokenType GS.RationalTok = FloatTok
    mapTokenType GS.SpaceTok = NormalTok
    mapTokenType GS.StringTok = StringTok
    mapTokenType GS.SymbolTok = OperatorTok
    mapTokenType GS.VariableTok = VariableTok

data CodeInputConfig t = CodeInputConfig
    { _codeInputConfig_initialValue :: CodeZipper TokenType
    , _codeInputConfig_value :: Maybe (Dynamic t (CodeZipper TokenType))
    , _codeInputConfig_modify :: Event t (CodeZipper TokenType -> CodeZipper TokenType)
    , _codeInputConfig_tabWidth :: Int
    , _codeInputConfig_display :: Dynamic t (Char -> Char)
    , _codeInputConfig_showCursor :: Bool
    }

instance Reflex t => Default (CodeInputConfig t) where
    def =
        CodeInputConfig
            { _codeInputConfig_initialValue = CZ.empty
            , _codeInputConfig_value = Nothing
            , _codeInputConfig_modify = never
            , _codeInputConfig_tabWidth = 4
            , _codeInputConfig_display = pure id
            , _codeInputConfig_showCursor = True
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
            let c = if cfg._codeInputConfig_showCursor then (`V.withStyle` V.reverseVideo) else id
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
    -> (V.Attr -> V.Attr)
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
    tokenToSpan CZ.Token{..} = highlightSpan tokenType $ Span tag tokenContent
    cursorTokenToSpan = tokenToSpan >>> position @1 %~ cursorTag
    lineToSpans line = tokenToSpan <$> (plainToken "" : line)
    (cursorToken, tokensAfter) = fromMaybe (plainToken " ", []) $ do
        (ta, tas) <- uncons z.tokensAfter
        let (ct, ta') = CZ.splitTokenAt 1 ta
        pure (ct, ta' : tas)
    currentLineSpans =
        mconcat
            [ lineToSpans $ reverse z.tokensBefore
            , [cursorTokenToSpan cursorToken]
            , lineToSpans tokensAfter
            ]

nonEmptyToken :: Token -> Token
nonEmptyToken t
    | Text.null t.tokenContent = t{CZ.tokenContent = " "}
    | otherwise = t

highlightSpan :: TokenType -> Span V.Attr -> Span V.Attr
highlightSpan = (position @1 %~) . tokenAttr @DefaultDark

tokenAttr :: forall base16. Base16 base16 => TokenType -> V.Attr -> V.Attr
tokenAttr KeywordTok = flip V.withForeColor (base0E @base16)
tokenAttr DataTypeTok = flip V.withForeColor (base0A @base16)
tokenAttr DecValTok = flip V.withForeColor (base09 @base16)
tokenAttr BaseNTok = flip V.withForeColor (base09 @base16)
tokenAttr FloatTok = flip V.withForeColor (base09 @base16)
tokenAttr ConstantTok = flip V.withForeColor (base09 @base16)
tokenAttr CharTok = flip V.withForeColor (base08 @base16)
tokenAttr SpecialCharTok = flip V.withForeColor (base0F @base16)
tokenAttr StringTok = flip V.withForeColor (base0B @base16)
tokenAttr VerbatimStringTok = flip V.withForeColor (base0B @base16)
tokenAttr SpecialStringTok = flip V.withForeColor (base0B @base16)
tokenAttr ImportTok = flip V.withForeColor (base0D @base16)
tokenAttr CommentTok = flip V.withForeColor (base03 @base16) . flip V.withStyle V.italic
tokenAttr DocumentationTok = flip V.withForeColor (base08 @base16)
tokenAttr AnnotationTok = flip V.withForeColor (base0F @base16)
tokenAttr CommentVarTok = flip V.withForeColor (base03 @base16) . flip V.withStyle V.italic
tokenAttr OtherTok = flip V.withForeColor (base0A @base16)
tokenAttr FunctionTok = flip V.withForeColor (base0D @base16)
tokenAttr VariableTok = flip V.withForeColor (base08 @base16)
tokenAttr ControlFlowTok = flip V.withForeColor (base0E @base16)
tokenAttr OperatorTok = flip V.withForeColor (base05 @base16)
tokenAttr BuiltInTok = flip V.withForeColor (base0D @base16)
tokenAttr ExtensionTok = flip V.withForeColor (base05 @base16)
tokenAttr PreprocessorTok = flip V.withForeColor (base0A @base16)
tokenAttr AttributeTok = flip V.withForeColor (base0A @base16)
tokenAttr RegionMarkerTok = flip V.withForeColor (base05 @base16)
tokenAttr InformationTok = flip V.withForeColor (base05 @base16)
tokenAttr WarningTok = flip V.withForeColor (base08 @base16)
tokenAttr AlertTok = flip V.withForeColor (base00 @base16)
tokenAttr ErrorTok = flip V.withForeColor (base00 @base16)
tokenAttr NormalTok = flip V.withForeColor (base05 @base16)

class Base16 base16 where
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
    base00 = V.linearColor @Word8 0x18 0x18 0x18
    base01 = V.linearColor @Word8 0x28 0x28 0x28
    base02 = V.linearColor @Word8 0x38 0x38 0x38
    base03 = V.linearColor @Word8 0x58 0x58 0x58
    base04 = V.linearColor @Word8 0xB8 0xB8 0xB8
    base05 = V.linearColor @Word8 0xD8 0xD8 0xD8
    base06 = V.linearColor @Word8 0xE8 0xE8 0xE8
    base07 = V.linearColor @Word8 0xF8 0xF8 0xF8
    base08 = V.linearColor @Word8 0xAB 0x46 0x42
    base09 = V.linearColor @Word8 0xDC 0x96 0x56
    base0A = V.linearColor @Word8 0xF7 0xCA 0x88
    base0B = V.linearColor @Word8 0xA1 0xB5 0x6C
    base0C = V.linearColor @Word8 0x86 0xC1 0xB9
    base0D = V.linearColor @Word8 0x7C 0xAF 0xC2
    base0E = V.linearColor @Word8 0xBA 0x8B 0xAF
    base0F = V.linearColor @Word8 0xA1 0x69 0x46

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
    -> (V.Attr -> V.Attr)
    -- ^ Metadata for the cursor
    -> CodeZipper TokenType
    -- ^ The text input contents and cursor state
    -> DisplayLines V.Attr
displayCodeLines = displayCodeLinesWithAlignment TextAlignment_Left

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
    V.EvKey V.KEnter [] -> CZ.insertChar '\n'
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
