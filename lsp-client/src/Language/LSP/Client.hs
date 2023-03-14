module Language.LSP.Client where

import Control.Concurrent.Async.Lifted (concurrently_, race)
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks, runReaderT)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (fromLeft)
import Data.Generics.Labels ()
import Language.LSP.Client.Decoding
import Language.LSP.Client.Encoding (encode)
import Language.LSP.Client.Session
import Language.LSP.VFS (initVFS)
import System.IO (Handle, stdin, stdout)
import UnliftIO.Exception (catch)
import Prelude

runSession :: Session () -> IO ()
runSession = runSessionWithHandles stdin stdout

runSessionWithHandles :: Handle -> Handle -> Session () -> IO ()
runSessionWithHandles input output action = initVFS $ \vfs -> do
    initialState <- defaultSessionState vfs
    flip runReaderT initialState $ do
        let stop :: Session () = do
                shouldStop <- asks shouldStop
                liftIO $ atomically $ readTVar shouldStop >>= check
        actionResult <- race (race stop (catch @_ @SomeException action $ error . show)) $ do
            let send = flip (catch @_ @SomeException) (error . show) $ do
                    message <- asks outgoing >>= liftIO . atomically . readTQueue
                    liftIO $ LazyByteString.hPut output $ encode message
            let receive = flip (catch @_ @SomeException) (error . show) $ do
                    serverBytes <- liftIO $ getNextMessage input
                    (serverMessage, callback) <-
                        asks pendingRequests
                            >>= liftIO . atomically . flip stateTVar (decodeFromServerMsg serverBytes)
                    handleServerMessage serverMessage
                    liftIO callback
            concurrently_ (forever send) (forever receive)
        case actionResult of
            Right _ -> error "send/receive thread should not exit!"
            Left (Left ()) ->
                -- Ended because `shutdown` was called
                pure ()
            Left (Right ()) ->
                -- Ended because action is over
                pure ()
