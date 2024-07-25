{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.LSP.SemanticTokensClock where

import Data.HashMap.Strict qualified as HashMap
import Language.LSP.Client.Session
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Prelude

data SemanticTokensClock = SemanticTokensClock

type SemanticTokens = LSP.SemanticTokens LSP.|? LSP.SemanticTokensDelta

instance (MonadIO m) => Clock (SessionT m) SemanticTokensClock where
    type Time SemanticTokensClock = UTCTime
    type
        Tag SemanticTokensClock =
            (LSP.VersionedTextDocumentIdentifier, SemanticTokens)
    initClock SemanticTokensClock = do
        resultIds <- newTVarIO $ HashMap.empty @LSP.TextDocumentIdentifier
        let clock = filterS . concatS . constM $ do
                threadDelay 1_000_000
                getAllVersionedDocs >>= mapM \doc -> do
                    let storeResultId t =
                            atomically . modifyTVar resultIds $
                                case t ^. LSP.resultId of
                                    Nothing -> HashMap.delete (doc ^. unversionedDoc)
                                    Just resultId -> HashMap.insert (doc ^. unversionedDoc) resultId
                    readTVarIO resultIds
                        >>= ( HashMap.lookup (doc ^. unversionedDoc) >>> \case
                                Nothing -> requestFullTokens doc
                                Just resultId -> requestTokensDelta doc resultId
                            )
                        >>= \case
                            Nothing -> pure Nothing
                            Just tokens -> do
                                storeResultId tokens
                                case tokens of
                                    LSP.InR (LSP.SemanticTokensDelta{_edits = []}) -> pure Nothing
                                    _ -> do
                                        time <- liftIO getCurrentTime
                                        pure $ Just (time, (doc, tokens))
        (clock,) <$> liftIO getCurrentTime

instance LSP.HasResultId SemanticTokens (Maybe Text) where
    resultId = lens getter setter
      where
        getter :: SemanticTokens -> Maybe Text
        getter (LSP.InL t) = t ^. LSP.resultId
        getter (LSP.InR t) = t ^. LSP.resultId
        setter :: SemanticTokens -> Maybe Text -> SemanticTokens
        setter (LSP.InL t) = LSP.InL . flip (set LSP.resultId) t
        setter (LSP.InR t) = LSP.InR . flip (set LSP.resultId) t

instance GetClockProxy SemanticTokensClock

requestFullTokens
    :: (MonadIO m)
    => LSP.VersionedTextDocumentIdentifier
    -> SessionT m (Maybe SemanticTokens)
requestFullTokens LSP.VersionedTextDocumentIdentifier{..} =
    request
        LSP.SMethod_TextDocumentSemanticTokensFull
        LSP.SemanticTokensParams
            { _textDocument = LSP.TextDocumentIdentifier{..}
            , _workDoneToken = Nothing
            , _partialResultToken = Nothing
            }
        >>= \case
            LSP.TResponseMessage{_result = Right (LSP.InL tokens)} -> pure . Just . LSP.InL $ tokens
            LSP.TResponseMessage{} -> pure Nothing

requestTokensDelta
    :: (MonadIO m)
    => LSP.VersionedTextDocumentIdentifier
    -> Text
    -> SessionT m (Maybe SemanticTokens)
requestTokensDelta LSP.VersionedTextDocumentIdentifier{..} _previousResultId =
    request
        LSP.SMethod_TextDocumentSemanticTokensFullDelta
        LSP.SemanticTokensDeltaParams
            { _previousResultId
            , _textDocument = LSP.TextDocumentIdentifier{..}
            , _workDoneToken = Nothing
            , _partialResultToken = Nothing
            }
        >>= \case
            LSP.TResponseMessage{_result = Right (LSP.InL tokens)} -> pure . Just . LSP.InL $ tokens
            LSP.TResponseMessage{_result = Right (LSP.InR (LSP.InL tokensDelta))} -> pure . Just . LSP.InR $ tokensDelta
            LSP.TResponseMessage{} -> pure Nothing
