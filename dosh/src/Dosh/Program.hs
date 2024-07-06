{-# LANGUAGE OverloadedLists #-}

module Dosh.Program where

import Control.Monad.Trans.Class (lift)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Rope.Zipper qualified as RopeZipper
import Data.Text.Utf16.Rope.Mixed qualified as MixedRope
import Dosh.LSP.DiagnosticsClock (DiagnosticsClock)
import Dosh.LSP.SemanticTokensClock (SemanticTokensClock)
import Dosh.LSP.Session (flowSession, runSession)
import Dosh.Prelude hiding (try)
import Dosh.Widgets.CodeInput
import FRP.Rhine
import FRP.Rhine.Terminal
    ( TerminalEventClock (TerminalEventClock)
    )
import Language.LSP.Client.Session
    ( SessionT
    , changeDoc
    , documentContents
    , openDoc
    )
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Types.Extra (partialTextDocumentContentChangeEvent)
import System.Terminal
    ( Interrupt (Interrupt)
    , MonadTerminal
    , TerminalT
    , runTerminalT
    , withTerminal
    )
import System.Terminal qualified as Terminal
import System.Terminal.Extra
import System.Terminal.Widgets.Common (Widget)
import System.Terminal.Widgets.Common qualified as Widget
import System.Terminal.Widgets.TextInput
import Prelude

data DoshState m = DoshState
    { input :: CodeInput (Attribute m)
    , active :: Bool
    , documentIdentifier :: LSP.TextDocumentIdentifier
    }
    deriving stock (Generic)

widget
    :: forall m w cl
     . ( MonadIO m
       , Tag cl ~ Tag TerminalEventClock
       , Widget w
       )
    => w
    -> ClSFExcept cl w w m (Either Interrupt w)
widget initialW = try . feedback initialW $ proc (w, x) -> do
    tag <- tagS -< ()
    case tag of
        Left Interrupt ->
            throwS -< Left Interrupt
        Right e
            | Just e == Widget.submitEvent w ->
                throwS -< Right w
            | otherwise ->
                returnA -< (Widget.handleEvent e w, x)

handleDiagnostics :: (Monad m) => ClSF m DiagnosticsClock st st
handleDiagnostics = returnA

handleSemanticTokens :: (Monad m) => ClSF m SemanticTokensClock st st
handleSemanticTokens = returnA

withClock
    :: ( cl ~ In cl
       , cl ~ Out cl
       )
    => cl
    -> ClSF m cl a b
    -> Rhine m cl a b
withClock = flip (@@)

doshPosition :: DoshState m -> LSP.Position
doshPosition = view ropeLspPosition . RopeZipper.cursor . (.input.input.value)

documentChanges
    :: (DoshState m, DoshState m)
    -> Terminal.Event
    -> [LSP.TextDocumentContentChangeEvent]
documentChanges (oldState, newState) (Terminal.KeyEvent Terminal.BackspaceKey []) =
    [ partialTextDocumentContentChangeEvent
        LSP.Range{_start = doshPosition newState, _end = doshPosition oldState}
        Nothing
        ""
    ]
documentChanges _ (Terminal.KeyEvent Terminal.DeleteKey []) = error "Not yet implemented"
documentChanges (doshPosition -> oldPos, _) (Terminal.KeyEvent (Terminal.CharKey k) []) =
    [ partialTextDocumentContentChangeEvent
        LSP.Range{_start = oldPos, _end = oldPos}
        Nothing
        (Text.singleton k)
    ]
documentChanges (doshPosition -> oldPos, _) (Terminal.KeyEvent Terminal.EnterKey []) =
    [ partialTextDocumentContentChangeEvent
        LSP.Range{_start = oldPos, _end = oldPos}
        Nothing
        "\n"
    ]
documentChanges _ _ = []

handleEvents
    :: (MonadIO m', m ~ TerminalT t (SessionT m'))
    => Rhine (ExceptT Interrupt m) TerminalEventClock (DoshState t') (DoshState t')
handleEvents = withClock TerminalEventClock $ proc st -> do
    tag <- tagS -< ()
    case tag of
        Left Interrupt ->
            throwS -< Interrupt
        Right e
            | Terminal.KeyEvent (Terminal.CharKey 'D') Terminal.ctrlKey == e ->
                throwS -< Interrupt
            | Just e == Widget.submitEvent st.input ->
                returnA -< st & #active .~ False
            | otherwise -> do
                let newState = st & #input %~ Widget.handleEvent e
                arrMCl (lift . lift . uncurry changeDoc)
                    -<
                        (st.documentIdentifier, documentChanges (st, newState) e)
                returnA -< newState

debugRh
    :: (MonadIO m', m ~ TerminalT t (SessionT m'), m'' ~ ExceptT Interrupt m)
    => Rhine m'' (HoistClock IO m'' (Millisecond 1000)) (DoshState t') ()
debugRh = withClock (ioClock waitClock) $ proc st -> do
    contents <- arrMCl (lift . lift . documentContents) -< st.documentIdentifier
    arrMCl (liftIO . Text.writeFile "lsp-contents.txt")
        -<
            MixedRope.toText $ fromMaybe "" contents

render
    :: (MonadTerminal m)
    => Rhine m (HoistClock IO m (Millisecond 16)) (DoshState t) ()
render = withClock (ioClock waitClock) . feedback Nothing $ proc (new, old) -> do
    arrMCl (uncurry Widget.render) -< (old <&> (.input), new.input)
    arrMCl (const Terminal.flush) -< ()
    returnA -< ((), Just new)

runDosh :: IO ()
runDosh =
    void . runSession . withTerminal . runTerminalT $ do
        uri <- lift $ openDoc "Foobar.hs" "haskell"
        flowSession
            (initialState uri)
            handleDiagnostics
            handleSemanticTokens
            (lift . lift)
            handleEvents
            (debugRh |@| render)
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
