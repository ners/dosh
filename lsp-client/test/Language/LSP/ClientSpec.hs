module Language.LSP.ClientSpec where

import Control.Monad
import Language.LSP.Client
import Language.LSP.Types
import System.IO
import System.Process (createPipe)
import Test.Hspec
import Prelude
import Control.Arrow ((>>>))
import UnliftIO (race, MonadUnliftIO, fromEither)
import UnliftIO.Concurrent
import Control.Exception

withTimeout :: forall m a. MonadUnliftIO m => Int -> m a -> m a
withTimeout delay a = fromEither =<< race timeout a
    where
        timeout = do
            threadDelay delay
            pure $ AssertionFailed "Timeout exceeded"

-- | LSP server that does not read input, and sends dummy diagnostics once per second
diagServer :: IO (Handle, Handle)
diagServer = do
    (_, inWrite) <- createPipe
    (outRead, outWrite) <- createPipe
    forkIO $ forM_ [1 ..] $ \i -> do
        threadDelay 1_000_000
        hPutStrLn outWrite $ "server diagnostics message " <> show i
    pure (inWrite, outRead)

-- | LSP server that accepts requests and answers them with a delay
reqServer :: IO (Handle, Handle)
reqServer = do
    (inRead, inWrite) <- createPipe
    (outRead, outWrite) <- createPipe
    forkIO $ forever $ do
        req <- hGetLine inRead
        let response = "req id ..."
        forkIO $ threadDelay 100_000 >> hPutStrLn outWrite response
    pure (inWrite, outRead)

-- | LSP client that waits for queries
client :: Handle -> Handle -> IO (Session () -> IO ())
client input output = do
    i <- newEmptyMVar
    o <- newEmptyMVar
    forkIO $ runSessionWithHandles input output $ forever $ do
        a <- takeMVar i
        a >>= putMVar o
    pure $ putMVar i >>> (*> readMVar o)

spec :: Spec
spec = do
    it "concurrently handles actions and server messages" $ do
        input <- reqServer >>= uncurry client
        threadDelay 1_000_000
        input $ do
            d <- withTimeout 1_000_000 getDiagnostics
            d `shouldBe` [] --server diagnostics message n
        threadDelay 1_000_000
        input $ do
            d <- withTimeout 1_000_000 getDiagnostics
            d `shouldBe` [] --server diagnostics message >n
    it "answers requests correctly" $ do
        input <- reqServer >>= uncurry client
        req1Done <- newEmptyMVar
        req1Id <- newEmptyMVar
        input $ do
            sendRequest SShutdown Empty (\reqId _ -> do
                (reqId `shouldBe`) =<< takeMVar req1Id
                putMVar req1Done ()) >>= putMVar req1Id
        req2Done <- newEmptyMVar
        req2Id <- newEmptyMVar
        input $ do
            sendRequest SShutdown Empty (\reqId _ -> do
                (reqId `shouldBe`) =<< takeMVar req2Id
                putMVar req2Done ()) >>= putMVar req2Id
        tryTakeMVar req1Done `shouldReturn` Nothing
        tryTakeMVar req2Done `shouldReturn` Nothing
        threadDelay 1_100_000
        tryTakeMVar req1Done `shouldReturn` Just ()
        tryTakeMVar req2Done `shouldReturn` Just ()
