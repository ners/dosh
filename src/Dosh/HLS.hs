{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.HLS (hls) where

import Control.Concurrent (forkIO)
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Development.IDE
import Development.IDE.Main (Command (LSP))
import Dosh.Server
import Ide.Arguments
import Ide.Main (runLspMode)
import Ide.Types (IdePlugins (IdePlugins))
import Reflex hiding (Query, Response)

hls
    :: forall t m
     . ( Reflex t
       , MonadIO m
       , PerformEvent t m
       , TriggerEvent t m
       , MonadIO (Performable m)
       , PostBuild t m
       , MonadFix m
       )
    => m (Client t)
hls = server $ \Server{..} -> do
    void $ liftIO $ do
        let minPriority = Warning
        recorder <-
            makeDefaultStderrRecorder Nothing minPriority
                <&> cfilter (\WithPriority{priority} -> priority >= minPriority)
                <&> cmapWithPrio pretty
        forkIO $ runLspMode recorder arguments plugins
    performEvent $
            onQuery <&> \Query{..} -> liftIO $ do
                respond EndResponse{..}

arguments :: GhcideArguments
arguments =
    GhcideArguments
        { argsCommand = LSP
        , argsCwd = Nothing
        , argsShakeProfiling = Nothing
        , argsTesting = True
        , argsExamplePlugin = False
        , argsDebugOn = True
        , argsLogFile = Nothing
        , argsThreads = 1
        , argsProjectGhcVersion = False
        }

plugins :: IdePlugins IdeState
plugins = IdePlugins []
