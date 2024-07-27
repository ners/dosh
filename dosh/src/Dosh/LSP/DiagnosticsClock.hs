module Dosh.LSP.DiagnosticsClock where

import Language.LSP.Client.Session
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Prelude

data DiagnosticsClock = DiagnosticsClock

instance (MonadIO m) => Clock (SessionT m) DiagnosticsClock where
    type Time DiagnosticsClock = UTCTime
    type
        Tag DiagnosticsClock =
            (LSP.VersionedTextDocumentIdentifier, [LSP.Diagnostic])
    initClock DiagnosticsClock = do
        diagnostics <- newTQueueIO
        receiveNotification LSP.SMethod_TextDocumentPublishDiagnostics \msg ->
            let diags = msg ^. LSP.params . LSP.diagnostics
                doc =
                    LSP.VersionedTextDocumentIdentifier
                        { _uri = msg ^. LSP.params . LSP.uri
                        , _version = fromMaybe 0 $ msg ^. LSP.params . LSP.version
                        }
             in atomically . writeTQueue diagnostics $ (doc, diags)
        let clock = constM do
                d <- atomically $ readTQueue diagnostics
                t <- liftIO getCurrentTime
                pure (t, d)
        (clock,) <$> liftIO getCurrentTime

instance GetClockProxy DiagnosticsClock
