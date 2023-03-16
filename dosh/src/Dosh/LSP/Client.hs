{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Dosh.LSP.Client where

import Data.Coerce (coerce)
import Development.IDE (WithPriority)
import Dosh.LSP.Document
import Dosh.LSP.Server (Server (..))
import Dosh.Prelude hiding (List)
import Language.LSP.Client.Session (Session)
import Language.LSP.Client.Session qualified as LSP
import Language.LSP.Types hiding (Initialize)
import Language.LSP.Types qualified as LSP
import Language.LSP.Types.Lens qualified as LSP
import Reflex hiding (Request, Response)
import Prelude hiding (id)

data Request
    = Initialize {}
    | CreateDocument {doc :: Document}
    | ChangeDocument {uri :: Uri, range :: Maybe Range, contents :: Text}
    | GetDocumentContents {uri :: Uri}
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
       , MonadIO m
       , MonadIO (Performable m)
       )
    => Server t
    -> m (Client t)
client server = do
    (onRequest, request) <- newTriggerEvent
    (onResponse, respond) <- newTriggerEvent
    liftIO . server.input $ do
        LSP.receiveNotification LSP.STextDocumentPublishDiagnostics $ \msg ->
            respond
                Diagnostics
                    { diagnostics = coerce $ msg ^. LSP.params . LSP.diagnostics
                    }
    performEvent $ liftIO . server.input . handleRequest respond <$> onRequest
    pure
        Client
            { request
            , onResponse
            , onError = server.error
            , onLog = server.log
            }

handleRequest :: (Response -> IO ()) -> Request -> Session ()
handleRequest _ Initialize{} = LSP.initialize
handleRequest _ CreateDocument{doc = Document{..}} = void $ LSP.createDoc (fromJust $ uriToFilePath uri) language contents
handleRequest _ ChangeDocument{..} =
    LSP.changeDoc
        (TextDocumentIdentifier uri)
        [ TextDocumentContentChangeEvent
            { _range = range
            , _rangeLength = Nothing
            , _text = contents
            }
        ]
handleRequest respond GetDocumentContents{uri} = do
    mc :: Maybe Text <- LSP.documentContents $ TextDocumentIdentifier uri
    forM_ mc $ \contents -> liftIO $ respond DocumentContents{..}
handleRequest respond GetCompletions{..} = void $ requestCompletions (TextDocumentIdentifier uri) position (liftIO . respond . Completions)

-- | Requests the completions for the position in the document.
requestCompletions :: TextDocumentIdentifier -> Position -> ([CompletionItem] -> IO ()) -> Session (LspId 'TextDocumentCompletion)
requestCompletions doc pos callback =
    LSP.sendRequest
        STextDocumentCompletion
        CompletionParams
            { _textDocument = doc
            , _position = pos
            , _workDoneToken = Nothing
            , _partialResultToken = Nothing
            , _context = Nothing
            }
        $ LSP.getResponseResult
            >>> \case
                InL (List items) -> items
                InR (CompletionList{_items = List items}) -> items
            >>> callback
