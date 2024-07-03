module Dosh.LSP.Session where

import Control.Monad.Schedule.Class (MonadSchedule)
import Control.Monad.Trans (MonadTrans)
import Data.ByteString (hGetSome, hPut)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Development.IDE (Recorder (..), WithPriority, cmapWithPrio)
import Development.IDE.Main (Arguments (..), defaultArguments, defaultMain)
import Dosh.LSP.DiagnosticsClock (DiagnosticsClock (DiagnosticsClock))
import Dosh.LSP.SemanticTokensClock (SemanticTokensClock (SemanticTokensClock))
import HlsPlugins (idePlugins)
import Language.LSP.Client (runSessionWithHandles)
import Language.LSP.Client.Session (Session, initialize)
import System.Process.Extra (createPipe)
import System.Terminal (TerminalT)
import Prelude

runSession :: (MonadIO m) => Session a -> m a
runSession actions = do
    (serverInput, serverOutput) <- liftIO $ do
        -- TODO: try to use Knob rather than pipes
        (inRead, inWrite) <- createLoggedPipe "lsp-input.log"
        hSetBuffering inRead NoBuffering
        hSetBuffering inWrite NoBuffering
        (outRead, outWrite) <- createLoggedPipe "lsp-output.log"
        hSetBuffering outRead NoBuffering
        hSetBuffering outWrite NoBuffering
        let logTrigger :: WithPriority Text -> IO ()
            logTrigger _ = pure ()
        let recorder :: Recorder (WithPriority Text)
            recorder = Recorder{logger_ = liftIO . logTrigger}
        forkIO $ ghcide recorder inRead outWrite
        pure (inWrite, outRead)
    liftIO . runSessionWithHandles serverOutput serverInput $ initialize >> actions

-- TODO: can we get rid of handles altogether?
ghcide :: Recorder (WithPriority Text) -> Handle -> Handle -> IO ()
ghcide recorder handleIn handleOut = defaultMain recorder' arguments
  where
    recorder' = cmapWithPrio (ishow . pretty) recorder
    plugins = idePlugins $ cmapWithPrio (ishow . pretty) recorder
    arguments =
        (defaultArguments recorder' plugins)
            { argsHandleIn = pure handleIn
            , argsHandleOut = pure handleOut
            }

createLoggedPipe :: FilePath -> IO (Handle, Handle)
createLoggedPipe logFile = do
    writeFile logFile ""
    (readEnd, writeEnd) <- createPipe
    (readEnd', writeEnd') <- createPipe
    hSetBuffering readEnd' NoBuffering
    hSetBuffering writeEnd NoBuffering
    forkIO . forever $ do
        c <- hGetSome readEnd' defaultChunkSize
        ByteString.appendFile logFile c
        hPut writeEnd c
    pure (readEnd, writeEnd')

runTerminalSession :: forall t a m. (MonadIO m) => TerminalT t Session a -> m a
runTerminalSession = undefined

flowSession
    :: forall m eventsCl renderCl st t
     . ( MonadIO m
       , Clock (TerminalT t Session) eventsCl
       , Clock (TerminalT t Session) (In eventsCl)
       , Clock (TerminalT t Session) (Out eventsCl)
       , GetClockProxy eventsCl
       , Time eventsCl ~ UTCTime
       , Time (In eventsCl) ~ Time eventsCl
       , Time (Out eventsCl) ~ Time eventsCl
       , Clock (TerminalT t Session) renderCl
       , Clock (TerminalT t Session) (In renderCl)
       , Clock (TerminalT t Session) (Out renderCl)
       , GetClockProxy renderCl
       , Time renderCl ~ UTCTime
       , Time (In renderCl) ~ Time eventsCl
       , Time (Out renderCl) ~ Time eventsCl
       , MonadSchedule (TerminalT t Session)
       )
    => st
    -> ClSF Session DiagnosticsClock st st
    -> ClSF Session SemanticTokensClock st st
    -> Rhine (TerminalT t Session) eventsCl st st
    -> Rhine (TerminalT t Session) renderCl st ()
    -> m ()
flowSession initialState diag sem eventsRh renderRh = runTerminalSession do
    let diagRh
            :: Rhine
                (TerminalT t Session)
                (LiftClock Session (TerminalT t) DiagnosticsClock)
                st
                st
        diagRh = liftClSFAndClock diag @@ liftClock DiagnosticsClock
        semRh
            :: Rhine
                (TerminalT t Session)
                (LiftClock Session (TerminalT t) SemanticTokensClock)
                st
                st
        semRh = liftClSFAndClock sem @@ liftClock SemanticTokensClock
        notificationsRh = feedbackify diagRh |@| feedbackify semRh
    flow $
        feedbackRhine
            (keepLast initialState)
            (notificationsRh |@| feedbackify eventsRh)
            >-- keepLast initialState
            --> renderRh

feedbackify :: (Monad m) => Rhine m cl a a -> Rhine m cl ((), a) (a, a)
feedbackify rh = snd ^>>@ rh @>>^ (\st -> (st, st))
