{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.LSP.Server where

import Data.ByteString (hGetSome, hPut)
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Development.IDE (Logger (Logger), Pretty (pretty), Recorder (..), WithPriority, cmapWithPrio, logWith)
import Development.IDE.Main (Arguments (..), defaultArguments, defaultMain)
import Dosh.Prelude
import Dosh.Util hiding (withTimeout)
import HlsPlugins (idePlugins)
import Language.LSP.Client qualified as LSP
import Language.LSP.Client.Session (Session)
import Reflex
    ( Reflex (Event)
    , TriggerEvent (newTriggerEvent)
    )
import System.Process (createPipe)

data Server t = Server
    { input :: Session () -> IO ()
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
    (serverInput, serverOutput) <- liftIO $ do
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
    sessionActions <- newChan
    liftIO $
        forkIO $
            LSP.runSessionWithHandles serverOutput serverInput $
                forever $ do
                    action <- readChan sessionActions
                    action `catch` (liftIO . reportError)
    pure
        Server
            { input = writeChan sessionActions
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
