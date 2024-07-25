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
import Data.List.Extra ((!?))
import Data.Position qualified as Position
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Utf16.Rope.Mixed qualified as MixedRope
import Dosh.App
import Dosh.LSP.DiagnosticsClock (DiagnosticsClock)
import Dosh.LSP.SemanticTokensClock (SemanticTokens, SemanticTokensClock)
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
    , openDoc
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

data DoshState m = DoshState
    { initializeResult :: LSP.InitializeResult
    , input :: CodeInput (Attribute m)
    , active :: Bool
    , documentIdentifier :: LSP.TextDocumentIdentifier
    }
    deriving stock (Generic)

handleDiagnostics :: (Monad m) => ClSF m DiagnosticsClock st st
handleDiagnostics = returnA

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
    :: (Monad m)
    => ( ((LSP.VersionedTextDocumentIdentifier, SemanticTokens), DoshState ann)
       , SemanticTokensState
       )
    -> m (DoshState ann, SemanticTokensState)
handleSemanticTokens (((doc, tokens), st), sts) = pure (st', sts')
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
            . #tokens
            .~ IntervalMap.fromList [(interval t, t._tokenType) | t <- abss]

handleSemanticTokensS
    :: (MonadIO m)
    => ClSF m SemanticTokensClock (DoshState ann) (DoshState ann)
handleSemanticTokensS = tagS &&& returnA >>> feedback HashMap.empty (arrMCl handleSemanticTokens)

withClock
    :: ( cl ~ In cl
       , cl ~ Out cl
       )
    => cl
    -> ClSF m cl a b
    -> Rhine m cl a b
withClock = flip (@@)

documentChanges
    :: (CodeInput m, CodeInput m)
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
    pos :: CodeInput m -> LSP.Position
    pos = view $ #input . #value . #cursor . position
    oldPos = pos oldInput
    newPos = pos newInput

handleEvents'
    :: Tag TerminalEventClock
    -> DoshState t
    -> AppExcept (DoshState t)
handleEvents' (Left Interrupt) _ = throwE Interrupt
handleEvents' (Right e) st
    | e == Terminal.KeyEvent (Terminal.CharKey 'D') Terminal.ctrlKey =
        throwE Interrupt
    | Just e == Widget.submitEvent st.input =
        pure $ st & #active .~ False
    | otherwise = do
        let newState = st & #input %~ Widget.handleEvent e
        mapM_
            (changeDoc st.documentIdentifier . pure)
            (documentChanges (st.input, newState.input) e)
        pure newState

handleEvents :: Rhine AppExcept TerminalEventClock (DoshState t') (DoshState t')
handleEvents =
    withClock TerminalEventClock $
        tagS &&& returnA >>> arrMCl (uncurry handleEvents')

writeDocumentContents
    :: Rhine AppExcept (HoistClock IO AppExcept (Millisecond 1000)) (DoshState t') ()
writeDocumentContents = withClock (ioClock waitClock) $ arrMCl \_ -> do
    docs <- getAllVersionedDocs
    contents <- forM docs \versionedDocId -> do
        let docId = LSP.TextDocumentIdentifier $ versionedDocId ^. LSP.uri
        contents <- documentContents docId
        pure $
            Text.unlines [ishow versionedDocId, MixedRope.toText . fromMaybe "" $ contents]
    liftIO . Text.writeFile "lsp-contents.txt" . Text.unlines $ contents

render
    :: Rhine AppExcept (HoistClock IO AppExcept (Millisecond 16)) (DoshState t) ()
render = withClock (ioClock waitClock) . feedback Nothing $ proc (new, old) -> do
    arrMCl (uncurry Widget.render) -< (old <&> (.input), new.input)
    arrMCl (const Terminal.flush) -< ()
    returnA -< ((), Just new)

runDosh :: IO ()
runDosh =
    void . runSession . withTerminal . runTerminalT . (.unApp) $ do
        initializeResult <- liftSession $ initialize (Just initializeOptions)
        uri <- liftSession $ openDoc "/tmp/dosh/Foobar.hs" "haskell"
        flowSession
            (initialState initializeResult uri)
            handleDiagnostics
            handleSemanticTokensS
            liftSession
            handleEvents
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
    initialState
        :: LSP.InitializeResult -> LSP.TextDocumentIdentifier -> DoshState m
    initialState initializeResult documentIdentifier =
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
                            , value = ""
                            }
                    , tokens = mempty
                    }
            , active = True
            , documentIdentifier
            }
