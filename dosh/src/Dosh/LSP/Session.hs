module Dosh.LSP.Session where

import Control.Monad.Schedule.Class (MonadSchedule)
import Data.Automaton.Trans.Except (reactimateExcept, try)
import Data.ByteString (hGetSome, hPut)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Development.IDE (Recorder (..), WithPriority, cmapWithPrio)
import Development.IDE.Main (Arguments (..), defaultArguments, defaultMain)
import Dosh.LSP.DiagnosticsClock (DiagnosticsClock (DiagnosticsClock))
import Dosh.LSP.SemanticTokensClock (SemanticTokensClock (SemanticTokensClock))
import HlsPlugins (idePlugins)
import Language.LSP.Client (runSessionWithHandles)
import Language.LSP.Client.Session (SessionT, initialize)
import System.Process.Extra (createPipe)
import Prelude

runSession :: (MonadUnliftIO m) => SessionT m a -> m a
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
    runSessionWithHandles serverOutput serverInput $ initialize >> actions

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

flowSession
    :: forall m m' e me' st cl cl'
     . ( Monad m
       , Clock m DiagnosticsClock
       , Clock m SemanticTokensClock
       , Monad m'
       , me' ~ ExceptT e m'
       , MonadSchedule m'
       , Clock me' cl
       , Clock me' (In cl)
       , Clock me' (Out cl)
       , GetClockProxy cl
       , Time cl ~ UTCTime
       , Time (In cl) ~ UTCTime
       , Time (Out cl) ~ UTCTime
       , Clock me' cl'
       , Clock me' (In cl')
       , Clock me' (Out cl')
       , GetClockProxy cl'
       , Time cl' ~ UTCTime
       , Time (In cl') ~ UTCTime
       )
    => st
    -> ClSF m DiagnosticsClock st st
    -> ClSF m SemanticTokensClock st st
    -> (forall a. m a -> me' a)
    -> Rhine me' cl st st
    -> Rhine me' cl' st ()
    -> m' e
flowSession initialState diagS semS monadMorphism otherRh sinkRh =
    flowExcept $
        feedbackRhine
            (keepLast initialState)
            ( feedbackify $ (diagRh |@| semRh) |@| otherRh
            )
            >-- keepLast initialState
            --> sinkRh
  where
    diagS' = hoistClSFAndClock monadMorphism diagS
    diagCl = HoistClock{unhoistedClock = DiagnosticsClock, monadMorphism}
    diagRh = diagS' @@ diagCl
    semS' = hoistClSFAndClock monadMorphism semS
    semCl = HoistClock{unhoistedClock = SemanticTokensClock, monadMorphism}
    semRh = semS' @@ semCl

feedbackify :: (Monad m) => Rhine m cl a a -> Rhine m cl ((), a) (a, a)
feedbackify rh = snd ^>>@ rh @>>^ (\st -> (st, st))

flowExcept
    :: ( Monad m
       , Clock (ExceptT e m) cl
       , GetClockProxy cl
       )
    => Rhine (ExceptT e m) cl () ()
    -> m e
flowExcept rhine =
    runExceptT (eraseClock rhine) >>= \case
        Left e -> pure e
        Right msf -> reactimateExcept . try $ msf >>> arr (const ())
