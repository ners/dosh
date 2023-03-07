{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.LSP.Server where

import Conduit (MonadCatch (catch))
import Control.Monad.Extra (whenM)
import Data.ByteString (hGetSome, hPut)
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Conduit.Parser (peek)
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Development.IDE (Logger (Logger), Pretty (pretty), Recorder (..), WithPriority, cmapWithPrio, logWith)
import Development.IDE.Main (Arguments (..), defaultArguments, defaultMain)
import Dosh.Prelude hiding (catch)
import Dosh.Util hiding (withTimeout)
import HlsPlugins (idePlugins)
import Language.LSP.Test (withTimeout)
import Language.LSP.Test qualified as LSP
import Reflex
    ( Reflex (Event)
    , TriggerEvent (newTriggerEvent)
    )
import System.Process (createPipe)

data Server t = Server
    { input :: LSP.Session () -> IO ()
    , output :: Handle
    , error :: Event t SomeException
    , log :: Event t (WithPriority Text)
    }

server
    :: forall t m
     . ( MonadIO m
       , TriggerEvent t m
       )
    => m (Server t)
server = do
    (log, logTrigger) <- newTriggerEvent
    (error, reportError) <- newTriggerEvent
    (input, output) <- liftIO $ do
        -- TODO: try to use Knob rather than pipes
        (inRead, inWrite) <- createPipe
        hSetBuffering inRead NoBuffering
        hSetBuffering inWrite NoBuffering
        (inRead', inWrite') <- createPipe
        hSetBuffering inRead' NoBuffering
        hSetBuffering inWrite' NoBuffering
        (outRead, outWrite) <- createPipe
        hSetBuffering outRead NoBuffering
        hSetBuffering outWrite NoBuffering
        (outRead', outWrite') <- createPipe
        hSetBuffering outRead' NoBuffering
        hSetBuffering outWrite' NoBuffering
        let recorder = Recorder{logger_ = liftIO . logTrigger}
        forkIO $ forever $ do
            c <- hGetSome inRead defaultChunkSize
            Text.appendFile "hls-input.log" $ Text.decodeUtf8 c
            hPut inWrite' c
        forkIO $ forever $ do
            c <- hGetSome outRead' defaultChunkSize
            Text.appendFile "hls-output.log" $ Text.decodeUtf8 c
            hPut outWrite c
        forkIO $ ghcide recorder inRead' outWrite'
        pure (inWrite, outRead)
    i <- newEmptyMVar
    o <- newEmptyMVar
    liftIO $ forkIO $ LSP.runSessionWithHandles input output LSP.defaultConfig LSP.fullCaps "." $ forever $ do
        let handleMessage = pure ()
        -- flip (catch @_ @SomeException) (const $ pure ()) $
        --    withTimeout 1000 $
        --        whenM (isJust <$> LSP.Session peek) $
        --            void LSP.anyMessage
        let handleInput = do
                a <- takeMVar i
                result <- a `catch` (liftIO . reportError)
                putMVar o result
        -- TODO: concurrently (forever handleMessage) (forever handleInput)
        handleMessage >> handleInput
    pure
        Server
            { input = putMVar i >>> (*> takeMVar o)
            , output
            , error
            , log
            }

-- TODO: can we get rid of handles altogether?
ghcide :: Recorder (WithPriority Text) -> Handle -> Handle -> IO ()
ghcide recorder stdin stdout = do
    let logger = Logger $ logWith recorder
        recorder' = cmapWithPrio (tshow . pretty) recorder
        plugins = idePlugins $ cmapWithPrio (tshow . pretty) recorder
        arguments =
            (defaultArguments recorder' logger plugins)
                { argsHandleIn = pure stdin
                , argsHandleOut = pure stdout
                }
    defaultMain recorder' arguments
