{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.LSP.Server where

import Development.IDE (Recorder (..), WithPriority, noLogging)
import Development.IDE.Main (Arguments (..), Log, defaultArguments, defaultMain)
import Dosh.Prelude
import Ide.Types (IdePlugins (IdePlugins))
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
    , log :: Event t (WithPriority Log)
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
        (outRead, outWrite) <- createPipe
        hSetBuffering outRead NoBuffering
        hSetBuffering outWrite NoBuffering
        let recorder = Recorder{logger_ = liftIO . logTrigger}
        forkIO $ ghcide recorder inRead outWrite
        pure (inWrite, outRead)
    i <- liftIO newEmptyMVar
    o <- liftIO newEmptyMVar
    liftIO $ forkIO $ LSP.runSessionWithHandles input output LSP.defaultConfig LSP.fullCaps "" $ forever $ do
        a <- liftIO $ takeMVar i
        result <- a `catch` (liftIO . reportError)
        liftIO $ putMVar o result
    pure
        Server
            { input = putMVar i >>> (*> takeMVar o)
            , output
            , error
            , log
            }

-- TODO: can we get rid of handles altogether?
ghcide :: Recorder (WithPriority Log) -> Handle -> Handle -> IO ()
ghcide recorder stdin stdout = do
    let logger = noLogging
        plugins :: IdePlugins ideState
        plugins = IdePlugins []
        arguments =
            (defaultArguments recorder logger plugins)
                { argsHandleIn = pure stdin
                , argsHandleOut = pure stdout
                }
    defaultMain recorder arguments
