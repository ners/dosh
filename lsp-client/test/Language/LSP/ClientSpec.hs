module Language.LSP.ClientSpec where

import Control.Arrow ((>>>))
import Control.Exception
import Control.Monad
import Control.Monad.Extra (whenMaybeM, whileJustM, whileM)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString, hGetSome)
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Development.IDE
import Development.IDE.Main
import HlsPlugins (idePlugins)
import Language.LSP.Client
import Language.LSP.Client.Decoding (getNextMessage)
import Language.LSP.Client.Encoding (encode)
import Language.LSP.Client.Session
import Language.LSP.Client.Session qualified as LSP
import Language.LSP.Types
import System.IO
import System.Process (createPipe)
import Test.Hspec hiding (shouldReturn)
import Test.Hspec qualified as Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
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
    hSetBuffering inRead LineBuffering
    hSetBuffering inWrite LineBuffering
    (outRead, outWrite) <- createPipe
    hSetBuffering outRead LineBuffering
    hSetBuffering outWrite LineBuffering
    threadId <- forkIO $ forM_ [1 ..] $ \i -> do
        threadDelay 1_000
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
    hSetBuffering inRead LineBuffering
    hSetBuffering inWrite LineBuffering
    (outRead, outWrite) <- createPipe
    hSetBuffering outRead LineBuffering
    hSetBuffering outWrite LineBuffering
    lock <- newMVar ()
    threadId <- forkIO $ forever $ do
        bytes <- liftIO $ getNextMessage inRead
        let obj = fromJust $ Aeson.decode bytes
        let idMaybe = parseMaybe (.: "id") obj
        let message = ResponseMessage "2.0" idMaybe (Right Empty) :: ResponseMessage 'Shutdown
        forkIO $ do
            threadDelay 1_000
            takeMVar lock
            LazyByteString.hPut outWrite $ encode message
            putMVar lock ()
    pure (inWrite, outRead, threadId)

-- | LSP server that accepts requests and answers them with a delay
hls :: IO (Handle, Handle, Handle, ThreadId)
hls = do
    (inRead, inWrite) <- createPipe
    hSetBuffering inRead LineBuffering
    hSetBuffering inWrite LineBuffering
    (outRead, outWrite) <- createPipe
    hSetBuffering outRead LineBuffering
    hSetBuffering outWrite LineBuffering
    (errRead, errWrite) <- createPipe
    hSetBuffering errRead LineBuffering
    hSetBuffering errWrite LineBuffering
    let recorder = Recorder{logger_ = liftIO . hPrint errWrite . payload}
        logger = Logger $ logWith recorder
        recorder' = cmapWithPrio (Text.pack . show . pretty) recorder
        plugins = idePlugins $ cmapWithPrio (Text.pack . show . pretty) recorder
        arguments =
            (defaultArguments recorder' logger plugins)
                { argsHandleIn = pure stdin
                , argsHandleOut = pure stdout
                }
    threadId <- forkIO $ defaultMain recorder' arguments
    pure (inWrite, outRead, errRead, threadId)

-- | LSP client that waits for queries
client :: Handle -> Handle -> IO (Session () -> IO (), ThreadId)
client serverInput serverOutput = do
    i <- newEmptyMVar
    o <- newEmptyMVar
    threadId <- forkIO $ runSessionWithHandles serverOutput serverInput $ forever $ do
        a <- takeMVar i
        a >>= putMVar o
    pure (putMVar i >>> (*> readMVar o), threadId)

getAvailableContents :: Handle -> IO ByteString
getAvailableContents h = whileJustM $ whenMaybeM (hReady h) (hGetSome h defaultChunkSize)

spec :: Spec
spec = do
    prop "concurrently handles actions and server messages" $ again $ do
        bracket
            diagServer
            (\(_, _, threadId) -> killThread threadId)
            $ \(serverIn, serverOut, _) -> runSessionWithHandles serverOut serverIn $ do
                -- We allow up to 0.1 s to receive the first batch of diagnostics
                withTimeout 100_000 $ whileM $ do
                    threadDelay 1_000
                    null <$> getDiagnostics
                [d1] <- getDiagnostics
                -- We allow up to 0.1 s to receive the next batch of diagnostics
                withTimeout 100_000 $ whileM $ do
                    threadDelay 1_000
                    [d2] <- getDiagnostics
                    pure $ d2._code == d1._code
    prop "answers requests correctly" $ again $ do
        bracket
            reqServer
            (\(_, _, threadId) -> killThread threadId)
            $ \(serverIn, serverOut, _) -> runSessionWithHandles serverOut serverIn $ do
                req1Done <- newEmptyMVar
                req1Id <- sendRequest SShutdown Empty (putMVar req1Done . (._id))
                req2Done <- newEmptyMVar
                req2Id <- sendRequest SShutdown Empty (putMVar req2Done . (._id))
                tryTakeMVar req1Done `shouldReturn` Nothing
                tryTakeMVar req2Done `shouldReturn` Nothing
                withTimeout 100_000 $ takeMVar req1Done `shouldReturn` Just req1Id
                withTimeout 5_000 $ takeMVar req2Done `shouldReturn` Just req2Id
    prop "opens and changes virtual documents correctly" $ do
        bracket
            hls
            ( \(_, _, serverErr, threadId) -> do
                killThread threadId
                getAvailableContents serverErr >>= hPutStrLn stderr . ByteString.unpack
            )
            $ \(serverIn, serverOut, _, _) -> runSessionWithHandles serverOut serverIn $ do
                uuid <- liftIO UUID.nextRandom
                let file = Text.unpack $ UUID.toText uuid <> ".hs"
                LSP.initialize
                threadDelay 100_000
                doc <- LSP.createDoc file "haskell" ""
                threadDelay 100_000
                LSP.documentContents doc `shouldReturn` (Just "")
