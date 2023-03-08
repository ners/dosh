{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.LSP.Client where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (fromLeft)
import Language.LSP.Client.Decoding
    ( RequestMap
    , decodeFromServerMsg
    , getNextMessage
    , newRequestMap
    )
import Language.LSP.Types
import System.IO (Handle, stdin, stdout)
import UnliftIO (MonadUnliftIO, concurrently_, liftIO, race)
import UnliftIO.STM
import Prelude

data SessionState = SessionState
    { pendingRequests :: TVar RequestMap
    , lastDiagnostics :: TVar [Diagnostic]
    , incoming :: TQueue LazyByteString
    , outgoing :: TQueue LazyByteString
    }

defaultSessionState :: MonadUnliftIO m => m SessionState
defaultSessionState = do
    pendingRequests <- newTVarIO newRequestMap
    lastDiagnostics <- newTVarIO []
    outgoing <- newTQueueIO
    incoming <- newTQueueIO
    pure SessionState{..}

class HasDiagnostics a where
    diagnostics :: Lens' a (Maybe [Diagnostic])

type Session = ReaderT SessionState IO

data ServerResponse

runSession :: Session a -> IO a
runSession = runSessionWithHandles stdin stdout

runSessionWithHandles :: Handle -> Handle -> Session a -> IO a
runSessionWithHandles input output action = do
    initialState <- defaultSessionState
    flip runReaderT initialState $ do
        actionResult <- race action $ do
            let send =
                    asks outgoing
                        >>= atomically . readTQueue
                        >>= liftIO . LazyByteString.hPut output
            let receive = do
                    serverBytes <- liftIO $ getNextMessage input
                    asks incoming >>= atomically . (`writeTQueue` serverBytes)
                    pendingRequests <- asks pendingRequests
                    serverMessage <- atomically $ do
                        requestMap <- readTVar pendingRequests
                        let (requestMap', serverMessage) = decodeFromServerMsg requestMap serverBytes
                        writeTVar pendingRequests requestMap'
                        pure serverMessage
                    case serverMessage of
                        FromServerMess _ _ -> pure () -- if diagnostics, update state
                        FromServerRsp _ _ -> pure () -- assign response to request
            concurrently_ (forever send) (forever receive)
        pure $ fromLeft undefined actionResult

sendRequest
    :: SClientMethod m
    -> MessageParams m
    -> (LspId m -> ResponseMessage m -> IO ())
    -> Session (LspId m)
sendRequest method params callback = undefined

--
-- sendNotification :: LspNotification -> Session ()
-- sendNotification notification = undefined
--
getDiagnostics :: Session [Diagnostic]
getDiagnostics = undefined

--
-- getDocumentContents :: Uri -> Session Text
-- getDocumentContents uri = undefined
