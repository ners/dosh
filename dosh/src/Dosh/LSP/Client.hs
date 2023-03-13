{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Dosh.LSP.Client where

import Development.IDE (WithPriority)
import Dosh.LSP.Server (Server (..))
import Dosh.Prelude hiding (List)
import Language.LSP.Client.Session (Session)
import Language.LSP.Client.Session qualified as LSP
import Language.LSP.Types hiding (Initialize)
import Reflex hiding (Request, Response)
import Prelude hiding (id)

data Request
    = Initialize {}
    | CreateDocument {uri :: Uri, language :: Text, contents :: Text}
    | ChangeDocument {uri :: Uri, range :: Range, contents :: Text}
    | GetDocumentContents {uri :: Uri}
    | GetDiagnostics {uri :: Uri}
    | GetCompletions {uri :: Uri, position :: Position}

data Response
    = DocumentContents {uri :: Uri, contents :: Text}
    | Diagnostics {diagnostics :: [Diagnostic]}
    | Completions {completions :: [CompletionItem]}

data Client t = Client
    { request :: Request -> IO ()
    , onResponse :: Event t Response
    , onError :: Event t SomeException
    , onLog :: Event t (WithPriority Text)
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
    performEvent $ liftIO . server.input . handleRequest respond <$> onRequest
    pure Client{onError = server.error, onLog = server.log, ..}

handleRequest :: (Response -> IO ()) -> Request -> Session ()
handleRequest _ Initialize{} = LSP.initialize
handleRequest _ CreateDocument{..} = void $ LSP.createDoc (show uri) language contents
handleRequest _ ChangeDocument{..} =
    LSP.sendNotification STextDocumentDidChange $
        DidChangeTextDocumentParams
            { _textDocument =
                VersionedTextDocumentIdentifier
                    { _uri = uri
                    , _version = Just 1
                    }
            , _contentChanges =
                List
                    [ TextDocumentContentChangeEvent
                        { _range = Just range
                        , _rangeLength = Nothing
                        , _text = contents
                        }
                    ]
            }
handleRequest respond GetDocumentContents{uri} = do
    contents <- LSP.documentContents $ TextDocumentIdentifier uri
    liftIO $ respond DocumentContents{..}
handleRequest respond GetDiagnostics{..} = LSP.getDiagnosticsFor (TextDocumentIdentifier uri) >>= liftIO . respond . Diagnostics
handleRequest respond GetCompletions{..} = LSP.getCompletions (TextDocumentIdentifier uri) position >>= liftIO . respond . Completions
