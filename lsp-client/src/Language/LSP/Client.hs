{-# LANGUAGE CPP #-}

module Language.LSP.Client where

import Control.Concurrent.Async.Lifted ( concurrently_, race )
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Lens ( (<&>), Lens' )
import Control.Monad (forever)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Coerce (coerce)
import Data.Either (fromLeft)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.IxMap (insertIxMap)
import Data.Maybe (fromMaybe, fromJust)
import Language.LSP.Client.Compat ( getCurrentProcessID )
import Language.LSP.Client.Decoding
import Language.LSP.Client.Encoding (encode)
import Language.LSP.Types
import Language.LSP.VFS (VFS, initVFS)
import System.IO (Handle, stdin, stdout)
import Prelude
import Language.LSP.Types.Capabilities (fullCaps)
import Language.LSP.Client.Exceptions
    ( SessionException(UnexpectedResponseError) )
import Control.Exception (throw)

data SessionState = SessionState
    { initialized :: TMVar InitializeResult
    , pendingRequests :: TVar RequestMap
    , lastRequestId :: TVar Int32
    , lastDiagnostics :: TVar [Diagnostic]
    , incoming :: TQueue LazyByteString
    -- ^ bytes that have been read from the input handle, but not yet parsed
    , outgoing :: TQueue LazyByteString
    -- ^ messages that have been serialised but not yet written to the output handle
    , vfs :: TVar VFS
    -- ^ virtual, in-memory file system of the files known to the LSP
    }

defaultSessionState :: VFS -> IO SessionState
defaultSessionState vfs' = do
    initialized <- newEmptyTMVarIO
    pendingRequests <- newTVarIO newRequestMap
    lastRequestId <- newTVarIO 0
    lastDiagnostics <- newTVarIO []
    outgoing <- newTQueueIO
    incoming <- newTQueueIO
    vfs <- newTVarIO vfs'
    pure SessionState{..}

class HasDiagnostics a where
    diagnostics :: Lens' a (Maybe [Diagnostic])

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
        actionResult <- race action $ do
            let send = do
                    message <- asks outgoing >>= liftIO . atomically . readTQueue
                    liftIO $ LazyByteString.hPut output message
            let receive = do
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
                                >>= liftIO . atomically . flip writeTVar (coerce msg._params._diagnostics)
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
    response <- request SInitialize InitializeParams
              { _workDoneToken = Nothing
              , _processId = Just $ fromIntegral pid
              , _clientInfo = Just ClientInfo{ _name = "lsp-client", _version = Just CURRENT_PACKAGE_VERSION}
              , _rootPath = Nothing
              , _rootUri = Nothing
              , _initializationOptions = Nothing
              , _capabilities = fullCaps
              , _trace = Just TraceOff
              , _workspaceFolders = Nothing
              }
    case response._result of
        Left e -> throw $ UnexpectedResponseError (SomeLspId $ fromJust response._id) e
        Right r -> asks initialized >>= liftIO . atomically . flip putTMVar r
    sendNotification SInitialized $ Just InitializedParams

getDiagnostics :: Session [Diagnostic]
getDiagnostics = asks lastDiagnostics >>= liftIO . readTVarIO

--
-- getDocumentContents :: Uri -> Session Text
-- getDocumentContents uri = undefined
