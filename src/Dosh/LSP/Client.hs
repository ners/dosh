{-# LANGUAGE PartialTypeSignatures #-}

module Dosh.LSP.Client where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.String (fromString)
import Language.LSP.Types (InitializeParams (..), LspId (..), MessageParams, Method, MethodType (Notification, Request), NotificationMessage (..), RequestMessage (..), SMethod (..))
import Language.LSP.Types.Capabilities (ClientCapabilities (..))

lspEncode :: ToJSON a => a -> LazyByteString
lspEncode a = header <> content
  where
    content = encode a
    header = fromString $ "Content-Length: " <> show contentLength <> "\r\n\r\n"
    contentLength = LBS.length content

lspNotification
    :: forall f m
     . (ToJSON (MessageParams m), ToJSON (MessageParams m))
    => SMethod (m :: Method f 'Notification)
    -> MessageParams m
    -> LazyByteString
lspNotification _method _params = lspEncode $ NotificationMessage{_jsonrpc = "2.0", _method, _params}

lspRequest
    :: forall f m
     . (ToJSON (MessageParams m), FromJSON (SMethod m))
    => LspId m
    -> SMethod (m :: Method f 'Request)
    -> MessageParams m
    -> LazyByteString
lspRequest _id _method _params = lspEncode $ RequestMessage{_jsonrpc = "2.0", ..}

initialize :: LazyByteString
initialize =
    lspRequest (IdInt 0) SInitialize $
        InitializeParams
            { _workDoneToken = Nothing
            , _processId = Nothing
            , _clientInfo = Nothing
            , _rootPath = Nothing
            , _rootUri = Nothing
            , _initializationOptions = Nothing
            , _capabilities =
                ClientCapabilities
                    { _workspace = Nothing
                    , _textDocument = Nothing
                    , _window = Nothing
                    , _general = Nothing
                    , _experimental = Nothing
                    }
            , _trace = Nothing
            , _workspaceFolders = Nothing
            }
