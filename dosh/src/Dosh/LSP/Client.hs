{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Dosh.LSP.Client where

import Development.IDE (WithPriority)
import Dosh.LSP.Server (Server (..))
import Dosh.Prelude hiding (List)
import Language.LSP.Client (Session)
import Language.LSP.Client qualified as LSP
import Language.LSP.Types
import Reflex hiding (Request, Response)
import Prelude hiding (id)

data Request
    = OpenDocument {uri :: Uri, language :: Text, text :: Text}
    | ChangeDocument {uri :: Uri, range :: Range, text :: Text}
    | GetDocumentContents {uri :: Uri}
    | GetDiagnostics {uri :: Uri}
    | GetCompletions {uri :: Uri, position :: Position}
    | WaitForDiagnostics

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
handleRequest _ OpenDocument{..} =
    sendNotification STextDocumentDidOpen $
        DidOpenTextDocumentParams
            TextDocumentItem
                { _uri = uri
                , _languageId = language
                , _version = 0
                , _text = text
                }
handleRequest _ ChangeDocument{..} =
    sendNotification STextDocumentDidChange $
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
                        , _text = text
                        }
                    ]
            }
handleRequest respond GetDocumentContents{uri} = do
    contents <- LSP.documentContents $ TextDocumentIdentifier uri
    liftIO $ respond DocumentContents{..}
handleRequest respond WaitForDiagnostics{} = LSP.waitForDiagnostics >>= liftIO . respond . Diagnostics
handleRequest respond GetDiagnostics{..} = LSP.getCurrentDiagnostics (TextDocumentIdentifier uri) >>= liftIO . respond . Diagnostics
handleRequest respond GetCompletions{..} = LSP.getCompletions (TextDocumentIdentifier uri) position >>= liftIO . respond . Completions
