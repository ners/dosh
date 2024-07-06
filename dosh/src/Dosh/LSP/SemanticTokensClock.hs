module Dosh.LSP.SemanticTokensClock where

import Data.HashMap.Strict qualified as HashMap
import Language.LSP.Client.Session
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Prelude

data SemanticTokensClock = SemanticTokensClock

instance (MonadIO m) => Clock (SessionT m) SemanticTokensClock where
    type Time SemanticTokensClock = UTCTime
    type
        Tag SemanticTokensClock =
            (LSP.TextDocumentIdentifier, LSP.SemanticTokensDelta)
    initClock SemanticTokensClock = do
        resultIds <- newTVarIO HashMap.empty
        let clock = filterS . concatS $ arrM \() -> do
                threadDelay 100_000
                getAllVersionedDocs >>= mapM \LSP.VersionedTextDocumentIdentifier{..} -> do
                    let doc = LSP.TextDocumentIdentifier{..}
                        storeResultId t =
                            atomically . modifyTVar resultIds $
                                case t ^. LSP.resultId of
                                    Nothing -> HashMap.delete doc
                                    Just resultId -> HashMap.insert doc resultId
                    readTVarIO resultIds
                        >>= ( HashMap.lookup doc >>> \case
                                Nothing -> requestFullTokens doc
                                Just resultId -> requestTokensDelta doc resultId
                            )
                        >>= \case
                            Nothing -> pure Nothing
                            Just delta -> do
                                time <- liftIO getCurrentTime
                                storeResultId delta
                                pure $ Just (time, (doc, delta))
        (clock,) <$> liftIO getCurrentTime

instance GetClockProxy SemanticTokensClock

-- todo: this could be an iso
tokensToTokenDelta :: LSP.SemanticTokens -> LSP.SemanticTokensDelta
tokensToTokenDelta tokens =
    LSP.makeSemanticTokensDelta
        LSP.SemanticTokens{_resultId = Nothing, _data_ = []}
        tokens
        & LSP.resultId
        .~ tokens
        ^. LSP.resultId

requestFullTokens
    :: (MonadIO m)
    => LSP.TextDocumentIdentifier
    -> SessionT m (Maybe LSP.SemanticTokensDelta)
requestFullTokens doc =
    request
        LSP.SMethod_TextDocumentSemanticTokensFull
        LSP.SemanticTokensParams
            { _textDocument = doc
            , _workDoneToken = Nothing
            , _partialResultToken = Nothing
            }
        >>= \case
            LSP.TResponseMessage{_result = Right (LSP.InL tokens)} -> pure . Just . tokensToTokenDelta $ tokens
            LSP.TResponseMessage{} -> pure Nothing

requestTokensDelta
    :: (MonadIO m)
    => LSP.TextDocumentIdentifier
    -> Text
    -> SessionT m (Maybe LSP.SemanticTokensDelta)
requestTokensDelta doc _previousResultId =
    request
        LSP.SMethod_TextDocumentSemanticTokensFullDelta
        LSP.SemanticTokensDeltaParams
            { _previousResultId
            , _textDocument = doc
            , _workDoneToken = Nothing
            , _partialResultToken = Nothing
            }
        >>= \case
            LSP.TResponseMessage{_result = Right (LSP.InL tokens)} -> pure . Just . tokensToTokenDelta $ tokens
            LSP.TResponseMessage{_result = Right (LSP.InR (LSP.InL tokensDelta))} -> pure $ Just tokensDelta
            LSP.TResponseMessage{} -> pure Nothing
