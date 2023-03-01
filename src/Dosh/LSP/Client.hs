{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Dosh.LSP.Client where

import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Dosh.LSP.Server (Server (..))
import Dosh.Prelude hiding (List)
import Language.LSP.Test (Session, sendNotification)
import Language.LSP.Types
import Reflex hiding (Request, Response)
import Prelude hiding (id)

data Request
    = OpenDocument {uid :: UUID, language :: Text, text :: Text}
    | ChangeDocument {uid :: UUID, range :: Range, text :: Text}
    | CheckDocument {uid :: UUID}

data Response
    = FullResponse {uid :: UUID, content :: Text}
    | PartialResponse {uid :: UUID, content :: Text}
    | EndResponse {uid :: UUID}

data Client t = Client
    { request :: Request -> IO ()
    , onResponse :: Event t Response
    }

client
    :: forall t m
     . ( PerformEvent t m
       , TriggerEvent t m
       , MonadIO (Performable m)
       )
    => Server t
    -> m (Client t)
client server = do
    (onRequest, request) <- newTriggerEvent
    (onResponse, respond) <- newTriggerEvent
    performEvent $ liftIO . server.input . handleRequest <$> onRequest
    pure Client{..}

handleRequest :: Request -> Session ()
handleRequest OpenDocument{..} =
    sendNotification STextDocumentDidOpen $
        DidOpenTextDocumentParams
            TextDocumentItem
                { _uri = Uri $ UUID.toText uid
                , _languageId = language
                , _version = 0
                , _text = text
                }
handleRequest ChangeDocument{..} =
    sendNotification STextDocumentDidChange $
        DidChangeTextDocumentParams
            { _textDocument =
                VersionedTextDocumentIdentifier
                    { _uri = Uri $ UUID.toText uid
                    , _version = Just 1
                    }
            , _contentChanges =
                List
                    [ TextDocumentContentChangeEvent
                        { _range = Just range
                        , _rangeLength = Nothing
                        , _text = text
                        }
                    ]
            }
handleRequest CheckDocument{} = pure ()
