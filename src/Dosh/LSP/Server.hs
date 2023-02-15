{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.LSP.Server where

import Control.Concurrent.Strict (forkIO)
import Control.Monad (forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Development.IDE (Recorder (..), WithPriority, noLogging)
import Development.IDE.Main (Arguments (..), Log, defaultArguments, defaultMain)
import Ide.Types (IdePlugins (IdePlugins))
import Reflex
    ( MonadHold
    , PerformEvent (Performable)
    , PostBuild
    , Reflex (Event)
    , TriggerEvent (newTriggerEvent)
    )
import System.IO (BufferMode (NoBuffering), Handle, hSetBuffering)
import System.Process (createPipe)

data Server t = Server
    { input :: Handle
    , output :: Handle
    , log :: Event t (WithPriority Log)
    }

server
    :: forall t m
     . ( Reflex t
       , MonadIO m
       , PerformEvent t m
       , TriggerEvent t m
       , MonadIO (Performable m)
       , PostBuild t m
       , MonadFix m
       , MonadHold t m
       )
    => m (Server t)
server = do
    (log, logTrigger) <- newTriggerEvent
    (input, output) <- liftIO $ do
        -- TODO: try to use Knob rather than pipes
        (inRead, inWrite) <- createPipe
        hSetBuffering inRead NoBuffering
        hSetBuffering inWrite NoBuffering
        (outRead, outWrite) <- createPipe
        hSetBuffering outRead NoBuffering
        hSetBuffering outWrite NoBuffering
        let recorder = Recorder{logger_ = liftIO . logTrigger}
        forkIO $
            forever $ -- remove when LSP stops crashing :-)
                ghcide recorder inRead outWrite
        pure (inWrite, outRead)
    pure Server{..}

-- TODO: can we get rid of handles altogether?
ghcide :: Recorder (WithPriority Log) -> Handle -> Handle -> IO ()
ghcide recorder stdin stdout = do
    let logger = noLogging
        plugins = IdePlugins []
        arguments =
            (defaultArguments recorder logger plugins)
                { argsHandleIn = pure stdin
                , argsHandleOut = pure stdout
                }
    defaultMain recorder arguments
