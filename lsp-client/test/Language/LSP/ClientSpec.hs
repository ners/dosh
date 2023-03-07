module Language.LSP.ClientSpec where

import Language.LSP.Client
import System.IO (Handle)
import Test.Hspec
import Test.QuickCheck
import Prelude

-- | LSP server that does not read input, and sends dummy diagnostics once per second
diagServer :: IO (Handle, Handle)
diagServer = do
    (_, inWrite) <- createPipe
    (outRead, outWrite) <- createPipe
    forkIO $ forM_ [1 ..] $ \i -> do
        threadDelay 1_000_000
        hPutLine outWrite $ "server diagnostics message " <> show i
    pure (inWrite, outRead)

-- | LSP server that accepts requests and answers them with a delay
reqServer :: IO (Handle, Handle)
reqServer = do
    (inRead, inWrite) <- createPipe
    (outRead, outWrite) <- createPipe
    forkIO $ forever $ do
        req <- hGetLine inRead
        let response = "req id ..."
        forkIO $ threadDelay 100_000 $ hPutLine outWrite response
    pure (inWrite, outRead)

-- | LSP client that waits for queries
client :: Handle -> Handle -> IO (Session a -> IO ())
client input output = do
    i <- newEmptyMVar
    o <- newEmptyMVar
    forkIO $ runSessionWithHandles input output $ forever $ do
        a <- takeMVar i
        join a >>= putMVar o
    pure $ putMVar i >>> (*> readMVar o)

spec :: Spec
spec = do
    it "concurrently handles actions and server messages" $ do
        (input, output) <- diagServer
        input <- client input output
        threadDelay 1_000_000
        input $ do
            d <- withTimeout 1_000_000 getDiagnostics
            d `shouldBe` "server diagnostics message n"
        threadDelay 1_000_000
        input $ do
            d <- withTimeout 1_000_000 getDiagnostics
            d `shouldBe` "server diagnostics message >n"
    it "answers requests correctly" $ do
        (input, output) <- reqServer
        input <- client input output
        req1 <- newEmptyMVar
        input $ sendRequest "request 1" $ \req resp -> do
            id req `shouldBe` id resp
            putMVar req1 ()
        -- atomically test these two together
        -- req1 is not yet full ...
        req2 <- newEmptyMVar
        input $ sendRequest "request 2" $ \req resp -> do
            id req `shouldBe` id resp
            putMVar req2 ()

-- req2 is not yet full ...
