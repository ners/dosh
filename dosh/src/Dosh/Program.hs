{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.Program where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Dosh.LSP.DiagnosticsClock (DiagnosticsClock)
import Dosh.LSP.SemanticTokensClock (SemanticTokensClock)
import Dosh.LSP.Session (flowSession)
import Dosh.Prelude hiding (try)
import FRP.Rhine
import FRP.Rhine.Terminal (TerminalEventClock (TerminalEventClock), flowTerminal)
import Language.LSP.Client.Session (Session)
import System.Terminal.Internal (LocalTerminal)
import System.Terminal
    ( Event
    , Interrupt (Interrupt)
    , MonadInput (awaitWith, setBracketedPasteMode)
    , runTerminalT
    , withTerminal, TerminalT
    )
import System.Terminal.Widgets.Common (Widget (handleEvent, submitEvent))

data DoshState = DoshState {}

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
            | Just e == submitEvent w ->
                throwS -< Right w
            | otherwise ->
                returnA -< (handleEvent e w, x)

handleDiagnostics :: ClSF Session DiagnosticsClock st st
handleDiagnostics = returnA

handleSemanticTokens :: ClSF Session SemanticTokensClock st st
handleSemanticTokens = returnA

handleEvents :: Rhine Session TerminalEventClock st st
handleEvents = returnA @@ TerminalEventClock

render :: Rhine (TerminalT LocalTerminal m) (Millisecond 100) st ()
render = arr (const ()) @@ waitClock

instance (MonadInput m) => MonadInput (ReaderT r m) where
    awaitWith :: (STM Interrupt -> STM Event -> STM a) -> ReaderT r m a
    awaitWith = lift . awaitWith
    setBracketedPasteMode :: Bool -> ReaderT r m ()
    setBracketedPasteMode = lift . setBracketedPasteMode

runDosh :: (MonadIO m) => m ()
runDosh = withTerminal . runTerminalT $ do
    -- flowTerminal
    --flowSession DoshState handleDiagnostics handleSemanticTokens handleEvents render
