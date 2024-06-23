module Dosh.LSP.Session where

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

flowSession
    :: ( Clock Session cl
       , Clock Session (Out cl)
       , GetClockProxy cl
       , Time cl ~ UTCTime
       , Time (In cl) ~ Time cl
       , Time (Out cl) ~ Time cl
       )
    => ClSF Session DiagnosticsClock () ()
    -> ClSF Session SemanticTokensClock () ()
    -> Rhine Session cl () ()
    -> IO ()
flowSession diag sem rh = runSession do
    let diagRh = diag @@ DiagnosticsClock
        semRh = sem @@ SemanticTokensClock
    flow $ (diagRh |@| semRh) |@| rh
