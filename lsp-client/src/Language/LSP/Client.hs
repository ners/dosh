{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.LSP.Client where

import Control.Concurrent.Async.Lifted (concurrently_, race)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
-- import Data.Map.Strict qualified as Map

-- import Data.Tree.Lens (root)

-- import System.Directory (canonicalizePath)

-- import System.FilePath.Glob qualified as Glob

import Control.Exception (SomeException, throw)
import Control.Lens hiding (List)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Coerce (coerce)
import Data.Either (fromLeft)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IxMap (insertIxMap)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Language.LSP.Client.Compat (getCurrentProcessID)
import Language.LSP.Client.Decoding
import Language.LSP.Client.Encoding (encode)
import Language.LSP.Client.Exceptions (SessionException (UnexpectedResponseError))
import Language.LSP.Types
import Language.LSP.Types.Capabilities (fullCaps)
import Language.LSP.Types.Lens hiding (applyEdit, capabilities, executeCommand, id, message, rename, to)
import Language.LSP.Types.Lens qualified
import Language.LSP.VFS
    ( VFS
    , initVFS
    , vfsMap
    , virtualFileText
    , virtualFileVersion
    )
import System.FilePath ((</>))
import System.IO (Handle, stdin, stdout)
import UnliftIO.Exception (catch)
import Prelude

data SessionState = SessionState
    { initialized :: TMVar InitializeResult
    , pendingRequests :: TVar RequestMap
    , lastRequestId :: TVar Int32
    , lastDiagnostics :: TVar (HashMap NormalizedUri [Diagnostic])
    , incoming :: TQueue LazyByteString
    -- ^ bytes that have been read from the input handle, but not yet parsed
    , outgoing :: TQueue LazyByteString
    -- ^ messages that have been serialized but not yet written to the output handle
    , vfs :: TVar VFS
    -- ^ virtual, in-memory file system of the files known to the LSP
    , rootDir :: FilePath
    }

defaultSessionState :: VFS -> IO SessionState
defaultSessionState vfs' = do
    initialized <- newEmptyTMVarIO
    pendingRequests <- newTVarIO newRequestMap
    lastRequestId <- newTVarIO 0
    lastDiagnostics <- newTVarIO mempty
    outgoing <- newTQueueIO
    incoming <- newTQueueIO
    vfs <- newTVarIO vfs'
    pure SessionState{rootDir = "", ..}

type Session = ReaderT SessionState IO

data ServerResponse

runSession :: Session a -> IO a
runSession = runSessionWithHandles stdin stdout

overTVar :: (a -> a) -> TVar a -> STM a
overTVar f var = stateTVar var (\x -> (f x, f x))

runSessionWithHandles :: Handle -> Handle -> Session a -> IO a
runSessionWithHandles input output action = initVFS $ \vfs -> do
    initialState <- defaultSessionState vfs
    flip runReaderT initialState $ do
        actionResult <- race (catch @_ @SomeException action $ error . show) $ do
            let send = flip (catch @_ @SomeException) (error . show) $ do
                    message <- asks outgoing >>= liftIO . atomically . readTQueue
                    liftIO $ LazyByteString.hPut output message
            let receive = flip (catch @_ @SomeException) (error . show) $ do
                    serverBytes <- liftIO $ getNextMessage input
                    asks incoming >>= liftIO . atomically . (`writeTQueue` serverBytes)
                    (serverMessage, callback) <-
                        asks pendingRequests
                            >>= liftIO
                                . atomically
                                . flip
                                    stateTVar
                                    ( \reqs ->
                                        let (newReqs, msg, callback) = decodeFromServerMsg reqs serverBytes
                                         in ((msg, callback), newReqs)
                                    )
                    case serverMessage of
                        FromServerMess STextDocumentPublishDiagnostics msg ->
                            asks lastDiagnostics
                                >>= liftIO
                                    . atomically
                                    . flip
                                        modifyTVar
                                        (HashMap.insert (toNormalizedUri $ msg ^. params . uri) (coerce $ msg ^. params . diagnostics))
                        _ -> liftIO callback
            concurrently_ (forever send) (forever receive)
        pure $ fromLeft (error "send/receive thread should not exit!") actionResult

-- | Sends a request to the server, with a callback that fires when the response arrives.
sendRequest
    :: (FromJSON (SMethod m), ToJSON (MessageParams m))
    => SClientMethod m
    -> MessageParams m
    -> (ResponseMessage m -> IO ())
    -> Session (LspId m)
sendRequest method params callback = do
    reqId <- asks lastRequestId >>= liftIO . atomically . overTVar (+ 1) <&> IdInt
    asks pendingRequests
        >>= void
            . liftIO
            . atomically
            . overTVar
                ( \requestMap ->
                    fromMaybe requestMap $ insertIxMap reqId Callback{..} requestMap
                )
    let message = encode $ RequestMessage "2.0" reqId method params
    asks outgoing >>= liftIO . atomically . (`writeTQueue` message)
    pure reqId

-- | Sends a request to the server and waits for its response.
request
    :: ( FromJSON (SMethod m)
       , ToJSON (MessageParams m)
       )
    => SClientMethod m
    -> MessageParams m
    -> Session (ResponseMessage m)
request method params = do
    done <- liftIO newEmptyMVar
    void $ sendRequest method params $ putMVar done
    liftIO $ takeMVar done

{- | Checks the response for errors and throws an exception if needed.
 Returns the result if successful.
-}
getResponseResult :: ResponseMessage m -> ResponseResult m
getResponseResult response = either err id $ response ^. result
  where
    lid = SomeLspId $ fromJust $ response ^. Language.LSP.Types.Lens.id
    err = throw . UnexpectedResponseError lid

-- | Sends a notification to the server.
sendNotification
    :: ToJSON (MessageParams m)
    => SClientMethod (m :: Method 'FromClient 'Notification)
    -> MessageParams m
    -> Session ()
sendNotification method params = do
    let message = encode $ NotificationMessage "2.0" method params
    asks outgoing >>= liftIO . atomically . (`writeTQueue` message)

initialize :: Session ()
initialize = do
    pid <- liftIO getCurrentProcessID
    response <-
        request
            SInitialize
            InitializeParams
                { _workDoneToken = Nothing
                , _processId = Just $ fromIntegral pid
                , _clientInfo = Just ClientInfo{_name = "lsp-client", _version = Just CURRENT_PACKAGE_VERSION}
                , _rootPath = Nothing
                , _rootUri = Nothing
                , _initializationOptions = Nothing
                , _capabilities = fullCaps
                , _trace = Just TraceOff
                , _workspaceFolders = Nothing
                }
    asks initialized >>= liftIO . atomically . flip putTMVar (getResponseResult response)
    sendNotification SInitialized $ Just InitializedParams

{- | Returns the current diagnostics that have been sent to the client.
 Note that this does not wait for more to come in.
-}
getDiagnosticsFor :: TextDocumentIdentifier -> Session [Diagnostic]
getDiagnosticsFor doc = asks lastDiagnostics >>= liftIO . readTVarIO <&> HashMap.lookupDefault [] (toNormalizedUri $ doc ^. uri)

-- | Returns the completions for the position in the document.
getCompletions :: TextDocumentIdentifier -> Position -> Session [CompletionItem]
getCompletions doc pos = do
    response <-
        request
            STextDocumentCompletion
            CompletionParams
                { _textDocument = doc
                , _position = pos
                , _workDoneToken = Nothing
                , _partialResultToken = Nothing
                , _context = Nothing
                }
    case getResponseResult response of
        InL (List items) -> pure items
        InR (CompletionList{_items = List items}) -> pure items

{- | /Creates/ a new text document. This is different from 'openDoc'
 as it sends a workspace/didChangeWatchedFiles notification letting the server
 know that a file was created within the workspace, __provided that the server
 has registered for it__, and the file matches any patterns the server
 registered for.
 It /does not/ actually create a file on disk, but is useful for convincing
 the server that one does exist.
-}

-- createDoc
--     :: FilePath
--     -- ^ The path to the document to open, __relative to the root directory__.
--     -> Text
--     -- ^ The text document's language identifier, e.g. @"haskell"@.
--     -> Text
--     -- ^ The content of the text document to create.
--     -> Session TextDocumentIdentifier
--     -- ^ The identifier of the document just created.
-- createDoc file languageId contents = do
--     dynCaps <- curDynCaps <$> get
--     rootDir <- asks rootDir
--     caps <- asks sessionCapabilities
--     absFile <- liftIO $ canonicalizePath (rootDir </> file)
--     let pred :: SomeRegistration -> [Registration WorkspaceDidChangeWatchedFiles]
--         pred (SomeRegistration r@(Registration _ SWorkspaceDidChangeWatchedFiles _)) = [r]
--         pred _ = mempty
--         regs = concatMap pred $ Map.elems dynCaps
--         watchHits :: FileSystemWatcher -> Bool
--         watchHits (FileSystemWatcher pattern kind) =
--             -- If WatchKind is excluded, defaults to all true as per spec
--             fileMatches (Text.unpack pattern) && createHits (fromMaybe (WatchKind True True True) kind)

--         fileMatches pattern = Glob.match (Glob.compile pattern) relOrAbs
--           where
--             -- If the pattern is absolute then match against the absolute fp
--             relOrAbs
--                 | isAbsolute pattern = absFile
--                 | otherwise = file

--         createHits (WatchKind create _ _) = create

--         regHits :: Registration WorkspaceDidChangeWatchedFiles -> Bool
--         regHits reg = foldl' (\acc w -> acc || watchHits w) False (reg ^. registerOptions . watchers)

--         clientCapsSupports =
--             caps
--                 ^? workspace
--                 . _Just
--                 . didChangeWatchedFiles
--                 . _Just
--                 . dynamicRegistration
--                 . _Just
--                 == Just True
--         shouldSend = clientCapsSupports && foldl' (\acc r -> acc || regHits r) False regs

--     when shouldSend $
--         sendNotification SWorkspaceDidChangeWatchedFiles $
--             DidChangeWatchedFilesParams $
--                 List [FileEvent (filePathToUri (rootDir </> file)) FcCreated]
--     openDoc' file languageId contents

{- | Opens a text document that /exists on disk/, and sends a
 textDocument/didOpen notification to the server.
-}
openDoc :: FilePath -> Text -> Session TextDocumentIdentifier
openDoc file languageId = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
    contents <- liftIO $ Text.readFile fp
    openDoc' file languageId contents

{- | This is a variant of `openDoc` that takes the file content as an argument.
 Use this is the file exists /outside/ of the current workspace.
-}
openDoc' :: FilePath -> Text -> Text -> Session TextDocumentIdentifier
openDoc' file languageId contents = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
        uri = filePathToUri fp
        item = TextDocumentItem uri languageId 0 contents
    sendNotification STextDocumentDidOpen (DidOpenTextDocumentParams item)
    pure $ TextDocumentIdentifier uri

-- | Closes a text document and sends a textDocument/didOpen notification to the server.
closeDoc :: TextDocumentIdentifier -> Session ()
closeDoc docId = do
    let params = DidCloseTextDocumentParams (TextDocumentIdentifier (docId ^. uri))
    sendNotification STextDocumentDidClose params

-- | Changes a text document and sends a textDocument/didOpen notification to the server.
changeDoc :: TextDocumentIdentifier -> [TextDocumentContentChangeEvent] -> Session ()
changeDoc docId changes = do
    verDoc <- getVersionedDoc docId
    let params = DidChangeTextDocumentParams (verDoc & version . non 0 +~ 1) (List changes)
    sendNotification STextDocumentDidChange params

-- | Gets the Uri for the file corrected to the session directory.
getDocUri :: FilePath -> Session Uri
getDocUri file = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
    return $ filePathToUri fp

-- | Returns the symbols in a document.
getDocumentSymbols :: TextDocumentIdentifier -> Session (Either [DocumentSymbol] [SymbolInformation])
getDocumentSymbols doc = do
    ResponseMessage _ rspLid res <- request STextDocumentDocumentSymbol (DocumentSymbolParams Nothing Nothing doc)
    case res of
        Right (InL (List xs)) -> return (Left xs)
        Right (InR (List xs)) -> return (Right xs)
        Left err -> throw (UnexpectedResponseError (SomeLspId $ fromJust rspLid) err)

-- | The current text contents of a document.
documentContents :: TextDocumentIdentifier -> Session Text
documentContents doc = do
    vfs <- asks vfs >>= liftIO . readTVarIO
    let Just file = vfs ^. vfsMap . at (toNormalizedUri (doc ^. uri))
    return (virtualFileText file)

-- | Adds the current version to the document, as tracked by the session.
getVersionedDoc :: TextDocumentIdentifier -> Session VersionedTextDocumentIdentifier
getVersionedDoc (TextDocumentIdentifier uri) = do
    vfs <- asks vfs >>= liftIO . readTVarIO
    let ver = vfs ^? vfsMap . ix (toNormalizedUri uri) . to virtualFileVersion
    return (VersionedTextDocumentIdentifier uri ver)
