{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Dosh.Program where

import Data.Aeson.QQ.Simple
import Data.Bits (Bits (testBit))
import Data.ExtendedReal qualified as Extended
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Interval ((<=..<))
import Data.IntervalMap.Strict qualified as IntervalMap
import Data.List qualified as List
import Data.List.Extra ((!?))
import Data.Position qualified as Position
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Utf16.Rope.Mixed qualified as MixedRope
import Data.Tuple.Extra (uncurry3)
import Dosh.App
import Dosh.LSP.DiagnosticsClock (DiagnosticsClock)
import Dosh.LSP.SemanticTokensClock (SemanticTokensClock)
import Dosh.LSP.Session (flowSession, runSession)
import Dosh.Widgets.CodeInput
import FRP.Rhine.Terminal
    ( TerminalEventClock (TerminalEventClock)
    )
import Language.LSP.Client.Session
    ( changeDoc
    , documentContents
    , getAllVersionedDocs
    , initialize
    , liftSession
    , openDoc'
    )
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import System.Terminal
    ( Interrupt (Interrupt)
    , runTerminalT
    , withTerminal
    )
import System.Terminal qualified as Terminal
import System.Terminal.Widgets.Common qualified as Widget
import System.Terminal.Widgets.TextInput
import Prelude

data DoshState = DoshState
    { initializeResult :: LSP.InitializeResult
    , input :: CodeInput
    , active :: Bool
    , documentIdentifier :: LSP.TextDocumentIdentifier
    }
    deriving stock (Generic)

handleDiagnosticsS
    :: (Monad m)
    => ClSF m DiagnosticsClock DoshState DoshState
handleDiagnosticsS = proc st -> do
    (doc, diagnostics) <- tagS -< ()
    if doc ^. unversionedDoc == st.documentIdentifier
        then do
            time <- absoluteS -< ()
            returnA
                -<
                    st
                        & #input
                        . withLastChange time
                        %~ #diagnostics
                        .~ List.sortBy diagOrd diagnostics
        else returnA -< st
  where
    diagOrd :: LSP.Diagnostic -> LSP.Diagnostic -> Ordering
    diagOrd d1 d2 =
        comparing (._range._start._line) d1 d2
            `thenCmp` comparing (._range._start._character) d2 d1
    thenCmp :: Ordering -> Ordering -> Ordering
    thenCmp EQ o2 = o2
    thenCmp o1 _ = o1

type SemanticTokensDocState = Seq LSP.UInt

type SemanticTokensState =
    HashMap LSP.TextDocumentIdentifier SemanticTokensDocState

applySemanticTokensEdits
    :: [LSP.SemanticTokensEdit]
    -> SemanticTokensDocState
    -> SemanticTokensDocState
applySemanticTokensEdits = flip . foldr $ \LSP.SemanticTokensEdit{..} stds ->
    let (before, after) =
            stds
                & Seq.splitAt (fromIntegral _start)
                & second (Seq.drop $ fromIntegral _deleteCount)
        middle = Seq.fromList $ fromMaybe [] _data_
     in before <> middle <> after

handleSemanticTokens
    :: Time SemanticTokensClock
    -> Tag SemanticTokensClock
    -> (DoshState, SemanticTokensState)
    -> (DoshState, SemanticTokensState)
handleSemanticTokens time (doc, tokens) (st, sts) = (st', sts')
  where
    applyTokens =
        case tokens of
            LSP.InL fullTokens -> const . Seq.fromList $ fullTokens._data_
            LSP.InR tokensDelta -> applySemanticTokensEdits tokensDelta._edits
    sts' =
        HashMap.alter
            (Just . applyTokens . fromMaybe [])
            (doc ^. unversionedDoc)
            sts
    legend =
        st.initializeResult
            ^? LSP.capabilities
            . LSP.semanticTokensProvider
            . _Just
            . LSP._L
            . LSP.legend
    uintToTokenTypes :: LSP.UInt -> Maybe LSP.SemanticTokenTypes
    uintToTokenTypes n = do
        tys <- view LSP.tokenTypes <$> legend
        LSP.fromOpenEnumBaseType <$> tys !? fromIntegral n
    uintToTokenModifiers :: LSP.UInt -> [LSP.SemanticTokenModifiers]
    uintToTokenModifiers n =
        case view LSP.tokenModifiers <$> legend of
            Just (fmap LSP.fromOpenEnumBaseType -> modifiers) ->
                snd
                    <$> filter (\(b, _) -> fromIntegral @_ @Int n `testBit` b) (zip [0 ..] modifiers)
            _ -> []
    parseRel :: Seq LSP.UInt -> LSP.SemanticTokenRelative
    parseRel
        [ _deltaLine
            , _deltaStartChar
            , _length
            , uintToTokenTypes -> fromMaybe (LSP.SemanticTokenTypes_Custom "") -> _tokenType
            , uintToTokenModifiers -> _tokenModifiers
            ] = LSP.SemanticTokenRelative{..}
    parseRel _ = error "wat"
    rels = parseRel <$> Seq.chunksOf 5 (sts' HashMap.! (doc ^. unversionedDoc))
    abss = LSP.absolutizeTokens . toList $ rels
    start :: Lens' LSP.SemanticTokenAbsolute LSP.Position
    start =
        lens
            (\LSP.SemanticTokenAbsolute{..} -> LSP.Position{_line, _character = _startChar})
            (\t LSP.Position{..} -> t & LSP.line .~ _line & LSP.startChar .~ _character)
    interval t =
        Extended.Finite (t ^. start)
            <=..< Extended.Finite (t ^. start & LSP.character +~ t._length)
    st' =
        st
            & #input
            . withLastChange time
            . #tokens
            .~ IntervalMap.fromList [(interval t, t._tokenType) | t <- abss]

handleSemanticTokensS
    :: (MonadIO m)
    => ClSF m SemanticTokensClock DoshState DoshState
handleSemanticTokensS = feedback HashMap.empty $ proc st -> do
    time <- absoluteS -< ()
    tag <- tagS -< ()
    returnA -< handleSemanticTokens time tag st

withClock
    :: ( cl ~ In cl
       , cl ~ Out cl
       )
    => cl
    -> ClSF m cl a b
    -> Rhine m cl a b
withClock = flip (@@)

documentChanges
    :: (CodeInput, CodeInput)
    -> Terminal.Event
    -> Maybe LSP.TextDocumentContentChangeEvent
documentChanges (oldInput, newInput) e =
    case e of
        Terminal.KeyEvent Terminal.BackspaceKey [] ->
            Just . LSP.TextDocumentContentChangeEvent . LSP.InL $
                LSP.TextDocumentContentChangePartial
                    { _rangeLength = Nothing
                    , _text = ""
                    , _range = LSP.Range{_start = newPos, _end = oldPos}
                    }
        Terminal.KeyEvent Terminal.DeleteKey [] ->
            Just . LSP.TextDocumentContentChangeEvent . LSP.InL $
                LSP.TextDocumentContentChangePartial
                    { _rangeLength = Nothing
                    , _text = ""
                    , _range =
                        LSP.Range
                            { _start = oldPos
                            , _end =
                                let
                                    oldLines = Widget.lineCount oldInput
                                    newLines = Widget.lineCount newInput
                                    deltaLines = oldLines - newLines
                                 in
                                    newPos
                                        & if deltaLines == 0
                                            then Position.col +~ 1
                                            else Position.row +~ deltaLines >>> Position.col .~ 0
                            }
                    }
        Terminal.KeyEvent (Terminal.CharKey k) [] ->
            Just . LSP.TextDocumentContentChangeEvent . LSP.InL $
                LSP.TextDocumentContentChangePartial
                    { _rangeLength = Nothing
                    , _text = Text.singleton k
                    , _range = LSP.Range{_start = oldPos, _end = oldPos}
                    }
        Terminal.KeyEvent Terminal.EnterKey [] ->
            Just . LSP.TextDocumentContentChangeEvent . LSP.InL $
                LSP.TextDocumentContentChangePartial
                    { _rangeLength = Nothing
                    , _text = "\n"
                    , _range = LSP.Range{_start = oldPos, _end = oldPos}
                    }
        _ -> Nothing
  where
    pos :: CodeInput -> LSP.Position
    pos = view $ #input . #value . #cursor . position
    oldPos = pos oldInput
    newPos = pos newInput

handleTerminalEvents
    :: Time TerminalEventClock
    -> Tag TerminalEventClock
    -> DoshState
    -> AppExcept DoshState
handleTerminalEvents _ (Left Interrupt) _ = throwE Interrupt
handleTerminalEvents time (Right e) st
    | e == Terminal.KeyEvent (Terminal.CharKey 'D') Terminal.ctrlKey =
        throwE Interrupt
    | Just e == Widget.submitEvent st.input =
        pure $ st & #active .~ False
    | otherwise = do
        let newState = st & #input . withLastChange time %~ Widget.handleEvent e
        mapM_
            (changeDoc st.documentIdentifier . pure)
            (documentChanges (st.input, newState.input) e)
        pure newState

handleTerminalEventsRh
    :: Rhine AppExcept TerminalEventClock DoshState DoshState
handleTerminalEventsRh = withClock TerminalEventClock $ proc st -> do
    time <- absoluteS -< ()
    tag <- tagS -< ()
    arrMCl (uncurry3 handleTerminalEvents) -< (time, tag, st)

writeDocumentContents
    :: Rhine AppExcept (HoistClock IO AppExcept (Millisecond 1000)) DoshState ()
writeDocumentContents =
    withClock (ioClock waitClock) . constM $
        getAllVersionedDocs >>= mapM_ \doc -> do
            contents <-
                MixedRope.toText . fromMaybe "" <$> documentContents (doc ^. unversionedDoc)
            mapM_ (liftIO . flip Text.writeFile contents . fromText)
                . Text.stripPrefix "file://"
                . LSP.getUri
                $ doc ^. LSP.uri

render
    :: Rhine AppExcept (HoistClock IO AppExcept (Millisecond 16)) DoshState ()
render = withClock (ioClock waitClock) . feedback Nothing $ proc (new, old) -> do
    arrMCl (uncurry Widget.render) -< (old <&> (.input), new.input)
    returnA -< ((), Just new)

runDosh :: IO ()
runDosh =
    void . runSession . withTerminal . runTerminalT . (.unApp) $ do
        state <- initialState
        flowSession
            state
            handleDiagnosticsS
            handleSemanticTokensS
            liftSession
            handleTerminalEventsRh
            (writeDocumentContents |@| render)
  where
    initializeOptions =
        [aesonQQ|
        {
            "plugin": {
                "semanticTokens": {
                    "globalOn": true
                }
            }
        }
        |]
    initialState :: App DoshState
    initialState = do
        initializeResult <- liftSession . initialize . Just $ initializeOptions
        let file :: FilePath
            file = "/tmp/dosh/Foobar.hs"
        contents <- liftIO $ Text.readFile file
        documentIdentifier <- liftSession $ openDoc' file "haskell" contents
        currentTime <- liftIO getCurrentTime
        pure
            DoshState
                { initializeResult
                , input =
                    CodeInput
                        { input =
                            TextInput
                                { valueTransform = id
                                , required = True
                                , prompt = "-> "
                                , multiline = True
                                , value = fromText contents
                                }
                        , tokens = mempty
                        , diagnostics = mempty
                        , lastChange = currentTime
                        }
                , active = True
                , documentIdentifier
                }
