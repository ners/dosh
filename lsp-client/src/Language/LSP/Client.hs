{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.LSP.Client where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Lens
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Coerce (coerce)
import Data.Either (fromLeft)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.IxMap (insertIxMap)
import Data.Maybe (fromMaybe)
import Language.LSP.Client.Decoding
    ( Callback (..)
    , RequestMap
    , decodeFromServerMsg
    , getNextMessage
    , newRequestMap
    )
import Language.LSP.Client.Encoding (encode)
import Language.LSP.Types
import Language.LSP.VFS (VFS, initVFS)
import System.IO (Handle, stdin, stdout)
import Prelude

data SessionState = SessionState
    { pendingRequests :: TVar RequestMap
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
                                >>= liftIO . atomically . flip writeTVar (enzyme msg)
                        _ -> pure ()
                    liftIO callback
            concurrently_ (forever send) (forever receive)
        pure $ fromLeft (error "send/receive thread should not exit!") actionResult
  where
    enzyme :: NotificationMessage 'TextDocumentPublishDiagnostics -> [Diagnostic]
    enzyme msg = coerce msg._params._diagnostics

sendRequest
    :: (FromJSON (SMethod m), ToJSON (MessageParams m))
    => SClientMethod m
    -> MessageParams m
    -> (ResponseMessage m -> IO ())
    -> Session ()
sendRequest method params callback = do
    reqId <- asks lastRequestId >>= liftIO . atomically . overTVar (+ 1) <&> IdInt

    let message = encode $ RequestMessage "2.0" reqId method params
    asks pendingRequests
        >>= void
            . liftIO
            . atomically
            . overTVar
                ( \requestMap ->
                    fromMaybe requestMap $ insertIxMap reqId Callback{..} requestMap
                )
    asks outgoing >>= liftIO . atomically . (`writeTQueue` message)

--
-- sendNotification :: LspNotification -> Session ()
-- sendNotification notification = undefined
--
getDiagnostics :: Session [Diagnostic]
getDiagnostics = asks lastDiagnostics >>= liftIO . readTVarIO

--
-- getDocumentContents :: Uri -> Session Text
-- getDocumentContents uri = undefined
