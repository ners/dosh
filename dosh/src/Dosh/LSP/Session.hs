module Dosh.LSP.Session where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Control.Monad.Schedule.Class (MonadSchedule)
import Data.Automaton.Trans.Except (reactimateExcept, try)
import Data.ByteString (hGetSome, hPut)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Default (Default (def))
import Data.Text.IO qualified as Text
import Development.IDE
    ( Priority (..)
    , Recorder (..)
    , WithPriority (..)
    , cmap
    , cmapWithPrio
    )
import Development.IDE.Main (Arguments (..), defaultArguments, defaultMain)
import Development.IDE.Session
    ( SessionLoadingOptions (findCradle, loadCradle)
    )
import Development.IDE.Session qualified as HLS
import Dosh.LSP.DiagnosticsClock (DiagnosticsClock (DiagnosticsClock))
import Dosh.LSP.SemanticTokensClock (SemanticTokensClock (SemanticTokensClock))
import GHC.IsList (IsList (fromList))
import HIE.Bios (Cradle)
import HIE.Bios qualified as Cradle
import HIE.Bios.Config qualified
import HIE.Bios.Config qualified as Cradle
import HIE.Bios.Cradle qualified as Cradle
import HIE.Bios.Types qualified
import HIE.Bios.Types qualified as Cradle
import HlsPlugins (idePlugins)
import Language.LSP.Client (runSessionWithHandles)
import Language.LSP.Client.Session (SessionT)
import System.Process.Extra (createPipe)
import Prelude

runSession :: (MonadUnliftIO m) => SessionT m a -> m a
runSession actions = do
    logChan <- newTChanIO
    void . forkIO . forever $
        atomically (readTChan logChan) >>= \WithPriority{..} ->
            liftIO . Text.appendFile "lsp-error.log" $
                mconcat [ishow priority, ": ", payload, "\n", ishow callStack_, "\n"]
    (serverInput, serverOutput) <- liftIO $ do
        -- TODO: try to use Knob rather than pipes
        (inRead, inWrite) <- createLoggedPipe "lsp-input.log"
        hSetBuffering inRead NoBuffering
        hSetBuffering inWrite NoBuffering
        (outRead, outWrite) <- createLoggedPipe "lsp-output.log"
        hSetBuffering outRead NoBuffering
        hSetBuffering outWrite NoBuffering
        let logTrigger :: WithPriority Text -> IO ()
            logTrigger = atomically . writeTChan logChan
        let recorder :: Recorder (WithPriority Text)
            recorder = Recorder{logger_ = liftIO . logTrigger}
        forkIO $ ghcide recorder inRead outWrite
        pure (inWrite, outRead)
    runSessionWithHandles serverOutput serverInput actions

-- TODO: can we get rid of handles altogether?
ghcide :: Recorder (WithPriority Text) -> Handle -> Handle -> IO ()
ghcide recorder handleIn handleOut = defaultMain recorder' arguments
  where
    recorder' = cmapWithPrio (ishow . pretty) recorder
    plugins = idePlugins $ cmapWithPrio (ishow . pretty) recorder
    severityToPriority :: Severity -> Priority
    severityToPriority Colog.Core.Debug = Development.IDE.Debug
    severityToPriority Colog.Core.Info = Development.IDE.Info
    severityToPriority Colog.Core.Warning = Development.IDE.Warning
    severityToPriority Colog.Core.Error = Development.IDE.Error
    withCradleToHlsLog :: WithSeverity Cradle.Log -> WithSeverity HLS.Log
    withCradleToHlsLog WithSeverity{..} = WithSeverity{getMsg = HLS.LogHieBios getMsg, ..}
    withSeverityToWithPriority :: WithSeverity a -> WithPriority a
    withSeverityToWithPriority WithSeverity{..} =
        WithPriority
            { priority = severityToPriority getSeverity
            , payload = getMsg
            , callStack_ = fromList []
            }
    rootFilePath :: FilePath
    rootFilePath = "/tmp/dosh"
    cradleAction :: b -> Cradle.CradleAction a
    cradleAction _ =
        Cradle.CradleAction
            { runGhcCmd = undefined
            , runCradle = \_ _ ->
                pure . Cradle.CradleSuccess $
                    Cradle.ComponentOptions
                        { componentRoot = rootFilePath
                        , componentOptions = ["-dynamic"]
                        , componentDependencies = []
                        }
            , actionName = HIE.Bios.Types.Direct
            }
    cradle :: (Show a) => Recorder (WithPriority HLS.Log) -> IO (Cradle a)
    cradle (cmap withSeverityToWithPriority -> cmap withCradleToHlsLog -> Recorder{..}) =
        Cradle.getCradle
            LogAction{unLogAction = logger_}
            cradleAction
            ( Cradle.CradleConfig
                { cradleDependencies = []
                , cradleTree = HIE.Bios.Config.Direct []
                }
            , rootFilePath
            )
    arguments =
        (defaultArguments recorder' rootFilePath plugins)
            { argsHandleIn = pure handleIn
            , argsHandleOut = pure handleOut
            , argsSessionLoadingOptions =
                def
                    { findCradle = const $ pure Nothing
                    , loadCradle = \r _ _ -> cradle r
                    }
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
