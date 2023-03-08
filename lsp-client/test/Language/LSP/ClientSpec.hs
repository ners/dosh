module Language.LSP.ClientSpec where

import Control.Arrow ((>>>))
import Control.Exception
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Language.LSP.Client
import Language.LSP.Client.Decoding (getNextMessage)
import Language.LSP.Types
import System.IO
import System.Process (createPipe)
import Test.Hspec
import UnliftIO (MonadIO (..), MonadUnliftIO, fromEither, race)
import UnliftIO.Concurrent
import Prelude hiding (log)
import Data.Aeson.Types (parseMaybe)

withTimeout :: forall m a. MonadUnliftIO m => Int -> m a -> m a
withTimeout delay a = fromEither =<< race timeout a
  where
    timeout = do
        threadDelay delay
        pure $ AssertionFailed "Timeout exceeded"

addHeader :: LazyByteString -> LazyByteString
addHeader content =
    mconcat
        [ "Content-Length: "
        , LazyByteString.pack $ show $ LazyByteString.length content
        , "\r\n"
        , "\r\n"
        , content
        ]

encode :: ToJSON a => a -> LazyByteString
encode = addHeader . Aeson.encode

data Config = Config
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

-- | LSP server that does not read input, and sends dummy diagnostics once per second
diagServer :: IO (Handle, Handle)
diagServer = do
    (_, inWrite) <- createPipe
    (outRead, outWrite) <- createPipe
    forkIO $ forM_ [1 ..] $ \i -> do
        threadDelay 1_000_000
        let message =
                NotificationMessage
                    "2.0"
                    STextDocumentPublishDiagnostics
                    PublishDiagnosticsParams
                        { _uri = Uri ""
                        , _version = Nothing
                        , _diagnostics = List []
                        }
        LazyByteString.hPut outWrite $ encode message
    pure (inWrite, outRead)

-- | LSP server that accepts requests and answers them with a delay
reqServer :: IO (Handle, Handle)
reqServer = do
    (inRead, inWrite) <- createPipe
    (outRead, outWrite) <- createPipe
    forkIO $ forever $ do
        bytes <- liftIO $ getNextMessage inRead
        let obj = fromJust $ Aeson.decode bytes
        let idMaybe = parseMaybe (.: "id") obj
        let message = ResponseMessage "2.0" idMaybe (Right Empty) :: ResponseMessage 'Shutdown
        forkIO $ do
            threadDelay 100_000
            LazyByteString.hPut outWrite $ encode message
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
            liftIO $ d `shouldBe` [] -- server diagnostics message n
        threadDelay 1_000_000
        input $ do
            d <- withTimeout 1_000_000 getDiagnostics
            liftIO $ d `shouldBe` [] -- server diagnostics message >n
    it "answers requests correctly" $ do
        input <- reqServer >>= uncurry client
        req1Done <- newEmptyMVar
        req1Id <- newEmptyMVar
        input $ do
            sendRequest
                SShutdown
                Empty
                ( \reqId _ -> do
                    (reqId `shouldBe`) =<< takeMVar req1Id
                    putMVar req1Done ()
                )
                >>= putMVar req1Id
        req2Done <- newEmptyMVar
        req2Id <- newEmptyMVar
        input $ do
            sendRequest
                SShutdown
                Empty
                ( \reqId _ -> do
                    (reqId `shouldBe`) =<< takeMVar req2Id
                    putMVar req2Done ()
                )
                >>= putMVar req2Id
        tryTakeMVar req1Done `shouldReturn` Nothing
        tryTakeMVar req2Done `shouldReturn` Nothing
        threadDelay 1_100_000
        tryTakeMVar req1Done `shouldReturn` Just ()
        tryTakeMVar req2Done `shouldReturn` Just ()
