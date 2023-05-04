{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Dosh.LSP.Client where

import Control.Concurrent.STM (flushTQueue)
import Control.Monad.Extra (whenJust, whenJustM)
import Control.Monad.State (execStateT)
import Data.Coerce (coerce)
import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Utf16.Rope (Rope)
import Development.IDE (WithPriority)
import Dosh.LSP.Document
import Dosh.LSP.Server (Server (..))
import Dosh.Prelude hiding (List)
import Language.LSP.Client.Session (Session)
import Language.LSP.Client.Session qualified as LSP
import Language.LSP.Types hiding (Initialize)
import Language.LSP.Types qualified as LSP
import Language.LSP.Types.Lens (uri)
import Language.LSP.Types.Lens qualified as LSP
import Reflex hiding (Request, Response)
import Prelude hiding (id)

data Request
    = Initialize {}
    | CreateDocument {document :: Document}
    | CloseDocument {identifier :: TextDocumentIdentifier}
    | ChangeDocument {identifier :: TextDocumentIdentifier, range :: Maybe Range, contents :: Text}
    | GetDocumentContents {identifier :: TextDocumentIdentifier}
    | GetCompletions {identifier :: TextDocumentIdentifier, position :: Position}

data Response
    = DocumentContents {identifier :: TextDocumentIdentifier, contents :: Rope}
    | Diagnostics {identifier :: TextDocumentIdentifier, diagnostics :: [Diagnostic]}
    | Completions {identifier :: TextDocumentIdentifier, completions :: [CompletionItem]}

data Client t = Client
    { request :: Request -> IO ()
    , onResponse :: Event t Response
    , onError :: Event t SomeException
    , onLog :: Event t (WithPriority Text)
    }
    deriving stock (Generic)

data DocumentState = DocumentState
    { document :: TVar Document
    , changes :: TQueue TextDocumentContentChangeEvent
    , updateThread :: ThreadId
    }
    deriving stock (Generic)

modifyTMVar :: TMVar a -> (a -> Maybe a) -> STM ()
modifyTMVar var f = readTMVar var >>= mapM_ (putTMVar var) . f

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
    documentStates <- newTVarIO HashMap.empty
    liftIO . server.input $ do
        LSP.receiveNotification LSP.STextDocumentPublishDiagnostics $ \msg -> do
            let identifier = TextDocumentIdentifier $ msg ^. LSP.params . LSP.uri
                diagnostics = coerce $ msg ^. LSP.params . LSP.diagnostics
            maybeDiagnostics <- withDocumentState documentStates identifier $ \doc -> normaliseDiagnostics <$> readTVar doc.document <*> pure diagnostics
            whenJust maybeDiagnostics $ \diagnostics ->
                respond Diagnostics{..}
    performEvent $ liftIO . server.input . handleRequest respond documentStates <$> onRequest
    pure
        Client
            { request
            , onResponse
            , onError = server.error
            , onLog = server.log
            }

tryReadTMVarIO :: MonadIO m => TMVar a -> m (Maybe a)
tryReadTMVarIO = atomically . tryReadTMVar

deriving stock instance Generic TextDocumentIdentifier

deriving anyclass instance Hashable TextDocumentIdentifier

withDocumentState :: MonadIO m => TVar (HashMap TextDocumentIdentifier DocumentState) -> TextDocumentIdentifier -> (DocumentState -> STM a) -> m (Maybe a)
withDocumentState docs identifier f = atomically $ readTVar docs <&> preview (at identifier . traverse) >>= mapM f

handleRequest :: (Response -> IO ()) -> TVar (HashMap TextDocumentIdentifier DocumentState) -> Request -> Session ()
handleRequest _ _ Initialize{} = LSP.initialize
handleRequest _ docs CreateDocument{document = document@Document{..}} = do
    document <- newTVarIO document
    changes <- newTQueueIO
    updateThread <- forkIO $ forever $ do
        changes <- atomically $ peekTQueue changes >> flushTQueue changes
        readTVarIO document
            >>= execStateT (handleUpdates changes)
            >>= atomically . writeTVar document
    atomically $ modifyTVar docs $ HashMap.insert identifier DocumentState{..}
    liftIO $ Text.appendFile "dosh.log" $ "LSP.createDoc " <> Text.pack (identifier ^. uri & fromJust . uriToFilePath)
    void $
        LSP.createDoc
            (identifier ^. uri & fromJust . uriToFilePath)
            language
            ""
handleRequest _ docs CloseDocument{..} = do
    withDocumentState docs identifier (pure . updateThread) >>= mapM_ killThread
    atomically $ modifyTVar docs $ HashMap.delete identifier
    LSP.closeDoc identifier
handleRequest _ docs ChangeDocument{..} =
    void $
        withDocumentState docs identifier $
            changes
                >>> flip
                    writeTQueue
                    TextDocumentContentChangeEvent
                        { _range = range
                        , _rangeLength = Nothing
                        , _text = contents
                        }
handleRequest respond _ GetDocumentContents{..} =
    whenJustM (LSP.documentContents identifier) $ \contents ->
        liftIO $ respond DocumentContents{..}
handleRequest respond _ GetCompletions{..} =
    void $ requestCompletions identifier position $ \completions ->
        liftIO $ respond Completions{..}

-- | Requests the completions for the position in the document.
requestCompletions
    :: TextDocumentIdentifier
    -> Position
    -> ([CompletionItem] -> IO ())
    -> Session (LspId 'TextDocumentCompletion)
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

-- | Returns the symbols in a document.
requestDocumentSymbols
    :: TextDocumentIdentifier
    -> (Either [DocumentSymbol] [SymbolInformation] -> IO ())
    -> Session (LspId 'TextDocumentDocumentSymbol)
requestDocumentSymbols doc callback = do
    LSP.sendRequest
        STextDocumentDocumentSymbol
        DocumentSymbolParams
            { _workDoneToken = Nothing
            , _partialResultToken = Nothing
            , _textDocument = doc
            }
        $ LSP.getResponseResult
            >>> \case
                InL (List xs) -> Left xs
                InR (List xs) -> Right xs
            >>> callback
