{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Client.Session where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (throw)
import Control.Lens hiding (List)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT, execState)
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Foldable (foldl', foldr', forM_, toList)
import Data.Function (on)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.IxMap (insertIxMap)
import Data.List (groupBy, sortBy)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics (Generic)
import Language.LSP.Client.Compat (getCurrentProcessID)
import Language.LSP.Client.Decoding
import Language.LSP.Client.Exceptions (SessionException (UnexpectedResponseError))
import Language.LSP.Types
import Language.LSP.Types.Capabilities (ClientCapabilities, fullCaps)
import Language.LSP.Types.Lens hiding (applyEdit, capabilities, executeCommand, id, message, rename, to)
import Language.LSP.Types.Lens qualified as LSP
import Language.LSP.VFS
    ( VFS
    , VfsLog
    , VirtualFile (..)
    , changeFromServerVFS
    , lsp_version
    , openVFS
    , vfsMap
    , virtualFileText
    , virtualFileVersion
    )
import System.Directory (canonicalizePath)
import System.FilePath (isAbsolute, (</>))
import System.FilePath.Glob qualified as Glob
import Prelude

deriving stock instance Generic ProgressToken

deriving anyclass instance Hashable ProgressToken

data SessionState = SessionState
    { initialized :: TMVar InitializeResult
    , pendingRequests :: TVar RequestMap
    , lastRequestId :: TVar Int32
    , lastDiagnostics :: TVar (HashMap NormalizedUri [Diagnostic])
    , serverCapabilities :: TVar (HashMap Text SomeRegistration)
    -- ^ The capabilities that the server has dynamically registered with us so
    -- far
    , clientCapabilities :: ClientCapabilities
    , progressTokens :: TVar (HashSet ProgressToken)
    , outgoing :: TQueue FromClientMessage
    -- ^ Messages that have been serialised but not yet written to the output handle
    , vfs :: TVar VFS
    -- ^ Virtual, in-memory file system of the files known to the LSP
    , rootDir :: FilePath
    }

defaultSessionState :: VFS -> IO SessionState
defaultSessionState vfs' = do
    initialized <- newEmptyTMVarIO
    pendingRequests <- newTVarIO newRequestMap
    lastRequestId <- newTVarIO 0
    lastDiagnostics <- newTVarIO mempty
    serverCapabilities <- newTVarIO mempty
    progressTokens <- newTVarIO mempty
    outgoing <- newTQueueIO
    vfs <- newTVarIO vfs'
    pure
        SessionState
            { rootDir = "."
            , clientCapabilities = def
            , ..
            }

type Session = ReaderT SessionState IO

-- extract Uri out from DocumentChange
-- didn't put this in `lsp-types` because TH was getting in the way
documentChangeUri :: DocumentChange -> Uri
documentChangeUri (InL x) = x ^. textDocument . uri
documentChangeUri (InR (InL x)) = x ^. uri
documentChangeUri (InR (InR (InL x))) = x ^. oldUri
documentChangeUri (InR (InR (InR x))) = x ^. uri

updateState :: FromServerMessage -> Session ()
updateState (FromServerMess SProgress req) = do
    let update = asks progressTokens >>= liftIO . atomically . flip modifyTVar (HashSet.insert $ req ^. params . token)
    case req ^. params . value of
        Begin{} -> update
        End{} -> update
        Report{} -> pure ()
updateState (FromServerMess SClientRegisterCapability req) = do
    let List newRegs = req ^. params . registrations <&> \sr@(SomeRegistration r) -> (r ^. LSP.id, sr)
    asks serverCapabilities >>= liftIO . atomically . flip modifyTVar (HashMap.union (HashMap.fromList newRegs))
updateState (FromServerMess SClientUnregisterCapability req) = do
    let List unRegs = req ^. params . unregisterations <&> (^. LSP.id)
    asks serverCapabilities >>= liftIO . atomically . flip modifyTVar (flip (foldr' HashMap.delete) unRegs)
updateState (FromServerMess STextDocumentPublishDiagnostics msg) =
    asks lastDiagnostics
        >>= liftIO
            . atomically
            . flip
                modifyTVar
                (HashMap.insert (toNormalizedUri $ msg ^. params . uri) (coerce $ msg ^. params . diagnostics))
updateState (FromServerMess SWorkspaceApplyEdit r) = do
    -- First, prefer the versioned documentChanges field
    allChangeParams <- case r ^. params . edit . documentChanges of
        Just (List cs) -> do
            mapM_ (checkIfNeedsOpened . documentChangeUri) cs
            -- replace the user provided version numbers with the VFS ones + 1
            -- (technically we should check that the user versions match the VFS ones)
            cs' <- traverseOf (traverse . _InL . textDocument) bumpNewestVersion cs
            return $ mapMaybe getParamsFromDocumentChange cs'
        -- Then fall back to the changes field
        Nothing -> case r ^. params . edit . changes of
            Just cs -> do
                mapM_ checkIfNeedsOpened (HashMap.keys cs)
                concat <$> mapM (uncurry getChangeParams) (HashMap.toList cs)
            Nothing ->
                error "WorkspaceEdit contains neither documentChanges nor changes!"

    asks vfs >>= liftIO . atomically . flip modifyTVar (execState $ changeFromServerVFS logger r)

    let groupedParams = groupBy (\a b -> a ^. textDocument == b ^. textDocument) allChangeParams
        mergedParams = map mergeParams groupedParams

    -- TODO: Don't do this when replaying a session
    forM_ mergedParams (sendNotification STextDocumentDidChange)

    -- Update VFS to new document versions
    let sortedVersions = map (sortBy (compare `on` (^. textDocument . version))) groupedParams
        latestVersions = map ((^. textDocument) . last) sortedVersions

    forM_ latestVersions $ \(VersionedTextDocumentIdentifier uri v) ->
        asks vfs
            >>= liftIO
                . atomically
                . flip
                    modifyTVar
                    ( \vfs -> do
                        let update (VirtualFile oldV file_ver t) = VirtualFile (fromMaybe oldV v) (file_ver + 1) t
                         in vfs & vfsMap . ix (toNormalizedUri uri) %~ update
                    )
  where
    logger :: LogAction (StateT VFS Identity) (WithSeverity VfsLog)
    logger = LogAction $ \(WithSeverity msg sev) -> case sev of Error -> error $ show msg; _ -> pure ()
    checkIfNeedsOpened uri = do
        isOpen <- asks vfs >>= liftIO . readTVarIO <&> has (vfsMap . ix (toNormalizedUri uri))

        -- if its not open, open it
        unless isOpen $ do
            let fp = fromJust $ uriToFilePath uri
            contents <- liftIO $ Text.readFile fp
            let item = TextDocumentItem (filePathToUri fp) "" 0 contents
                msg = NotificationMessage "2.0" STextDocumentDidOpen (DidOpenTextDocumentParams item)
            sendMessage $ fromClientNot msg
            asks vfs >>= liftIO . atomically . flip modifyTVar (execState $ openVFS logger msg)

    getParamsFromTextDocumentEdit :: TextDocumentEdit -> DidChangeTextDocumentParams
    getParamsFromTextDocumentEdit (TextDocumentEdit docId (List edits)) = do
        DidChangeTextDocumentParams docId (List $ map editToChangeEvent edits)

    editToChangeEvent :: TextEdit |? AnnotatedTextEdit -> TextDocumentContentChangeEvent
    editToChangeEvent (InR e) = TextDocumentContentChangeEvent (Just $ e ^. range) Nothing (e ^. newText)
    editToChangeEvent (InL e) = TextDocumentContentChangeEvent (Just $ e ^. range) Nothing (e ^. newText)

    getParamsFromDocumentChange :: DocumentChange -> Maybe DidChangeTextDocumentParams
    getParamsFromDocumentChange (InL textDocumentEdit) = Just $ getParamsFromTextDocumentEdit textDocumentEdit
    getParamsFromDocumentChange _ = Nothing

    bumpNewestVersion (VersionedTextDocumentIdentifier uri _) = head <$> textDocumentVersions uri

    -- For a uri returns an infinite list of versions [n,n+1,n+2,...]
    -- where n is the current version
    textDocumentVersions :: Uri -> Session [VersionedTextDocumentIdentifier]
    textDocumentVersions uri = do
        vfs <- asks vfs >>= liftIO . readTVarIO
        let curVer = fromMaybe 0 $ vfs ^? vfsMap . ix (toNormalizedUri uri) . lsp_version
        pure $ map (VersionedTextDocumentIdentifier uri . Just) [curVer + 1 ..]

    textDocumentEdits uri edits = do
        vers <- textDocumentVersions uri
        pure $ zipWith (\v e -> TextDocumentEdit v (List [InL e])) vers edits

    getChangeParams uri (List edits) = fmap getParamsFromTextDocumentEdit <$> textDocumentEdits uri (reverse edits)

    mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
    mergeParams params =
        let events = concat $ toList $ toList . (^. contentChanges) <$> params
         in DidChangeTextDocumentParams (head params ^. textDocument) (List events)
updateState _ = pure ()

overTVar :: (a -> a) -> TVar a -> STM a
overTVar f var = stateTVar var (\x -> (f x, f x))

-- | Sends a request to the server, with a callback that fires when the response arrives.
sendRequest
    :: forall m
     . Message m ~ RequestMessage m
    => SClientMethod m
    -> MessageParams m
    -> (ResponseMessage m -> IO ())
    -> Session (LspId m)
sendRequest method params callback = do
    reqId <- asks lastRequestId >>= liftIO . atomically . overTVar (+ 1) <&> IdInt
    asks pendingRequests
        >>= liftIO
            . atomically
            . flip
                modifyTVar
                ( \requestMap ->
                    fromMaybe requestMap $ insertIxMap reqId Callback{..} requestMap
                )
    sendMessage $ fromClientReq $ RequestMessage "2.0" reqId method params
    pure reqId

-- | Sends a request to the server and waits for its response.
request
    :: forall m
     . Message m ~ RequestMessage m
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
    lid = SomeLspId $ fromJust $ response ^. LSP.id
    err = throw . UnexpectedResponseError lid

-- | Sends a notification to the server.
sendNotification
    :: forall (m :: Method 'FromClient 'Notification)
     . Message m ~ NotificationMessage m
    => SClientMethod m
    -> MessageParams m
    -> Session ()
sendNotification method params = sendMessage $ fromClientNot $ NotificationMessage "2.0" method params

sendMessage :: FromClientMessage -> Session ()
sendMessage msg = asks outgoing >>= liftIO . atomically . (`writeTQueue` msg)

initialize :: Session ()
initialize = do
    pid <- liftIO getCurrentProcessID
    response <-
        request
            SInitialize
            InitializeParams
                { _workDoneToken = Nothing
                , _processId = Just $ fromIntegral pid
                , _clientInfo =
                    Just
                        ClientInfo
                            { _name = "lsp-client"
                            , _version = Just CURRENT_PACKAGE_VERSION
                            }
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
getDiagnostics :: Session [Diagnostic]
getDiagnostics = asks lastDiagnostics >>= liftIO . readTVarIO <&> concatMap snd . HashMap.toList

{- | Returns the current diagnostics for a given document that have been sent to the client.
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
createDoc
    :: FilePath
    -- ^ The path to the document to open, __relative to the root directory__.
    -> Text
    -- ^ The text document's language identifier, e.g. @"haskell"@.
    -> Text
    -- ^ The content of the text document to create.
    -> Session TextDocumentIdentifier
    -- ^ The identifier of the document just created.
createDoc file language contents = do
    serverCaps <- asks serverCapabilities >>= liftIO . readTVarIO
    clientCaps <- asks clientCapabilities
    rootDir <- asks rootDir
    absFile <- liftIO $ canonicalizePath (rootDir </> file)
    let pred :: SomeRegistration -> [Registration 'WorkspaceDidChangeWatchedFiles]
        pred (SomeRegistration r@(Registration _ SWorkspaceDidChangeWatchedFiles _)) = [r]
        pred _ = mempty
        regs :: [Registration 'WorkspaceDidChangeWatchedFiles]
        regs = concatMap pred $ HashMap.elems serverCaps
        watchHits :: FileSystemWatcher -> Bool
        watchHits (FileSystemWatcher pattern kind) =
            -- If WatchKind is excluded, defaults to all true as per spec
            fileMatches (Text.unpack pattern) && maybe True (view watchCreate) kind

        fileMatches pattern = Glob.match (Glob.compile pattern) (if isAbsolute pattern then absFile else file)

        regHits :: Registration 'WorkspaceDidChangeWatchedFiles -> Bool
        regHits reg = foldl' (\acc w -> acc || watchHits w) False (reg ^. registerOptions . watchers)

        clientCapsSupports =
            clientCaps
                ^? workspace
                    . _Just
                    . didChangeWatchedFiles
                    . _Just
                    . dynamicRegistration
                    . _Just
                == Just True
        shouldSend = clientCapsSupports && foldl' (\acc r -> acc || regHits r) False regs

    when shouldSend $
        sendNotification SWorkspaceDidChangeWatchedFiles $
            DidChangeWatchedFilesParams $
                List [FileEvent (filePathToUri (rootDir </> file)) FcCreated]
    openDoc' file language contents

{- | Opens a text document that /exists on disk/, and sends a
 textDocument/didOpen notification to the server.
-}
openDoc :: FilePath -> Text -> Session TextDocumentIdentifier
openDoc file language = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
    contents <- liftIO $ Text.readFile fp
    openDoc' file language contents

{- | This is a variant of `openDoc` that takes the file content as an argument.
 Use this is the file exists /outside/ of the current workspace.
-}
openDoc' :: FilePath -> Text -> Text -> Session TextDocumentIdentifier
openDoc' file language contents = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
        uri = filePathToUri fp
        item = TextDocumentItem uri language 0 contents
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
