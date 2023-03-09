module Language.LSP.ClientSpec where

import Control.Arrow ((>>>))
import Control.Exception
import Control.Monad
import Control.Monad.Extra ()
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (fromJust)
import Language.LSP.Client
import Language.LSP.Client.Decoding (getNextMessage)
import Language.LSP.Client.Encoding (encode)
import Language.LSP.Types
import System.IO
import System.Process (createPipe)
import Test.Hspec hiding (shouldReturn)
import Test.Hspec qualified as Hspec
import UnliftIO (MonadIO (..), MonadUnliftIO, fromEither, race)
import UnliftIO.Concurrent
import Prelude hiding (log)

shouldReturn :: (MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturn a expected = a >>= liftIO . flip Hspec.shouldBe expected

withTimeout :: forall m a. MonadUnliftIO m => Int -> m a -> m a
withTimeout delay a = fromEither =<< race timeout a
  where
    timeout = do
        threadDelay delay
        pure $ AssertionFailed "Timeout exceeded"

diagnostic :: Int -> Diagnostic
diagnostic i =
    Diagnostic
        { _range =
            Range
                { _start = Position{_line = 0, _character = 0}
                , _end = Position{_line = 0, _character = 0}
                }
        , _severity = Nothing
        , _code = Just $ InL $ fromIntegral i
        , _source = Nothing
        , _message = ""
        , _tags = Nothing
        , _relatedInformation = Nothing
        }

-- | LSP server that does not read input, and sends dummy diagnostics once per second
diagServer :: IO (Handle, Handle, ThreadId)
diagServer = do
    (inRead, inWrite) <- createPipe
    hSetBuffering inRead NoBuffering
    hSetBuffering inWrite NoBuffering
    (outRead, outWrite) <- createPipe
    hSetBuffering outRead NoBuffering
    hSetBuffering outWrite NoBuffering
    threadId <- forkIO $ forM_ [1 ..] $ \i -> do
        threadDelay 100_000
        let message =
                NotificationMessage
                    "2.0"
                    STextDocumentPublishDiagnostics
                    PublishDiagnosticsParams
                        { _uri = Uri ""
                        , _version = Nothing
                        , _diagnostics = List [diagnostic i]
                        }
        LazyByteString.hPut outWrite $ encode message
    pure (inWrite, outRead, threadId)

-- | LSP server that accepts requests and answers them with a delay
reqServer :: IO (Handle, Handle, ThreadId)
reqServer = do
    (inRead, inWrite) <- createPipe
    hSetBuffering inRead NoBuffering
    hSetBuffering inWrite NoBuffering
    (outRead, outWrite) <- createPipe
    hSetBuffering outRead NoBuffering
    hSetBuffering outWrite NoBuffering
    threadId <- forkIO $ forever $ do
        bytes <- liftIO $ getNextMessage inRead
        let obj = fromJust $ Aeson.decode bytes
        let idMaybe = parseMaybe (.: "id") obj
        let message = ResponseMessage "2.0" idMaybe (Right Empty) :: ResponseMessage 'Shutdown
        forkIO $ do
            threadDelay 100_000
            LazyByteString.hPut outWrite $ encode message
    pure (inWrite, outRead, threadId)

-- | LSP client that waits for queries
client :: Handle -> Handle -> IO (Session () -> IO (), ThreadId)
client serverInput serverOutput = do
    i <- newEmptyMVar
    o <- newEmptyMVar
    threadId <- forkIO $ runSessionWithHandles serverOutput serverInput $ forever $ do
        a <- takeMVar i
        a >>= putMVar o
    pure (putMVar i >>> (*> readMVar o), threadId)

spec :: Spec
spec = do
    it "concurrently handles actions and server messages" $ do
        (serverIn, serverOut, serverThread) <- diagServer
        flip finally (killThread serverThread) $ runSessionWithHandles serverOut serverIn $ do
            -- TODO: we should probably do something smarter than sleep
            getDiagnostics `shouldReturn` []
            threadDelay 110_000
            getDiagnostics `shouldReturn` [diagnostic 1]
            threadDelay 110_000
            getDiagnostics `shouldReturn` [diagnostic 2]
    it "answers requests correctly" $ do
        (serverIn, serverOut, serverThread) <- reqServer
        (clientIn, clientThread) <- client serverIn serverOut
        flip finally (mapM_ killThread [serverThread, clientThread]) $ do
            req1Done <- newEmptyMVar
            clientIn $ void $ sendRequest SShutdown Empty (putMVar req1Done . (._id))
            req2Done <- newEmptyMVar
            clientIn $ void $ sendRequest SShutdown Empty (putMVar req2Done . (._id))
            tryTakeMVar req1Done `shouldReturn` Nothing
            tryTakeMVar req2Done `shouldReturn` Nothing
            threadDelay 110_000
            tryTakeMVar req1Done `shouldReturn` Just (Just (IdInt 1))
            tryTakeMVar req2Done `shouldReturn` Just (Just (IdInt 2))
