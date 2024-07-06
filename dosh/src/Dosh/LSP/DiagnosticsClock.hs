module Dosh.LSP.DiagnosticsClock where

import Language.LSP.Client.Session
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Prelude

data DiagnosticsClock = DiagnosticsClock

instance (MonadIO m) => Clock (SessionT m) DiagnosticsClock where
    type Time DiagnosticsClock = UTCTime
    type Tag DiagnosticsClock = (LSP.TextDocumentIdentifier, [LSP.Diagnostic])
    initClock DiagnosticsClock = do
        diagnostics <- newTQueueIO
        receiveNotification LSP.SMethod_TextDocumentPublishDiagnostics \msg ->
            let diags = msg ^. LSP.params . LSP.diagnostics
                doc = LSP.TextDocumentIdentifier{_uri = msg ^. LSP.params . LSP.uri}
             in atomically . writeTQueue diagnostics $ (doc, diags)
        let clock = arrM \() -> do
                d <- atomically $ readTQueue diagnostics
                t <- liftIO getCurrentTime
                pure (t, d)
        (clock,) <$> liftIO getCurrentTime

instance GetClockProxy DiagnosticsClock
