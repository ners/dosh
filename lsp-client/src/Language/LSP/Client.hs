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

runSession :: Session a -> IO a
runSession = runSessionWithHandles stdin stdout

runSessionWithHandles :: Handle -> Handle -> Session a -> IO a
runSessionWithHandles input output action = initVFS $ \vfs -> do
    initialState <- defaultSessionState vfs
    flip runReaderT initialState $ do
        actionResult <- race (catch @_ @SomeException action $ error . show) $ do
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
        pure $ fromLeft (error "send/receive thread should not exit!") actionResult
