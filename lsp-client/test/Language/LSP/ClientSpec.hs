module Language.LSP.ClientSpec where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as Colog
import Control.Arrow ((>>>))
import Control.Exception
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, fromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Language.LSP.Client
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Server (Handlers, LspM, ServerDefinition (..), defaultOptions, runServerWithHandles)
import Language.LSP.Types
import System.IO
import System.Process (createPipe)
import Test.Hspec
import UnliftIO (MonadIO (..), MonadUnliftIO, fromEither, race)
import UnliftIO.Concurrent
import UnliftIO.STM
import Prelude hiding (log)

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

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

-- | LSP server that does not read input, and sends dummy diagnostics once per second
diagServer :: IO (Handle, Handle)
diagServer = do
    (inRead, inWrite) <- createPipe
    (outRead, outWrite) <- createPipe
    let
        -- Three loggers:
        -- 1. To stderr
        -- 2. To the client (filtered by severity)
        -- 3. To both
        stderrLogger :: LogAction IO (WithSeverity Text)
        stderrLogger = Colog.cmap show Colog.logStringStderr
        clientLogger :: LogAction (LspM Config) (WithSeverity Text)
        clientLogger = defaultClientLogger
        dualLogger :: LogAction (LspM Config) (WithSeverity Text)
        dualLogger = clientLogger <> Colog.hoistLogAction liftIO stderrLogger
        logToText = Text.pack . show . pretty

        serverDefinition =
            ServerDefinition
                { defaultConfig = Config{fooTheBar = False, wibbleFactor = 0}
                , onConfigurationChange = \_old v -> do
                    case fromJSON v of
                        Aeson.Error e -> Left (Text.pack e)
                        Aeson.Success cfg -> Right cfg
                , doInitialize = undefined
                , staticHandlers = mempty
                , interpretHandler = undefined
                , options = defaultOptions
                }
    forkIO $
        void $
            runServerWithHandles
                (Colog.cmap (fmap logToText) stderrLogger)
                (Colog.cmap (fmap logToText) dualLogger)
                inRead
                outWrite
                serverDefinition
    pure (inWrite, outRead)

-- | LSP server that accepts requests and answers them with a delay
reqServer :: IO (Handle, Handle)
reqServer = do
    (inRead, inWrite) <- createPipe
    (outRead, outWrite) <- createPipe
    forkIO $ forever $ do
        req <- hGetLine inRead
        let message = ResponseMessage "2.0" (Just $ IdInt 0) (Right Empty)
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
            d `shouldBe` [] -- server diagnostics message n
        threadDelay 1_000_000
        input $ do
            d <- withTimeout 1_000_000 getDiagnostics
            d `shouldBe` [] -- server diagnostics message >n
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
