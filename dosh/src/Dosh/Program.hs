{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Dosh.Program where

import Data.Position qualified as Position
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Utf16.Rope.Mixed qualified as MixedRope
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
    { input :: CodeInput (Attribute m)
    , active :: Bool
    , documentIdentifier :: LSP.TextDocumentIdentifier
    }
    deriving stock (Generic)

handleDiagnostics :: (Monad m) => ClSF m DiagnosticsClock st st
handleDiagnostics = returnA

handleSemanticTokens
    :: (MonadIO m)
    => (LSP.TextDocumentIdentifier, LSP.SemanticTokensDelta)
    -> st
    -> m st
handleSemanticTokens (_, LSP.SemanticTokensDelta{_edits}) st = do
    -- TODO parse semantic tokens
    -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
    liftIO . Text.appendFile "semantic-tokens.txt" $ ishow _edits <> "\n\n"
    pure st

handleSemanticTokensS :: (MonadIO m) => ClSF m SemanticTokensClock st st
handleSemanticTokensS = tagS &&& returnA >>> arrMCl (uncurry handleSemanticTokens)

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
        uri <- liftSession $ openDoc "/tmp/dosh/Foobar.hs" "haskell"
        flowSession
            (initialState uri)
            handleDiagnostics
            handleSemanticTokensS
            liftSession
            handleEvents
            (writeDocumentContents |@| render)
  where
    initialState :: LSP.TextDocumentIdentifier -> DoshState m
    initialState documentIdentifier =
        DoshState
            { input =
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
