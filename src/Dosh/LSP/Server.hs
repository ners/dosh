{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
 Module      : Dosh.LSP.Server

 A Reflex node that talks to the Haskell Language Server.
-}
module Dosh.LSP.Server where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.Strict
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.ByteString.Lazy (hPut)
import Data.Functor
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Development.IDE
import Development.IDE.Main (Arguments (..), Log, defaultArguments, defaultMain)
import Dosh.LSP.Client (initialize)
import Dosh.Server
import Dosh.Util
import Ide.Types (IdePlugins (IdePlugins))
import Reflex hiding (Query, Response)
import System.IO (BufferMode (NoBuffering), Handle, hSetBuffering)
import System.Process (createPipe)

languageServer
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
languageServer = server $ \Server{..} -> do
    let recorder = Recorder{logger_ = liftIO . log}
    (stdin, stdout) <- liftIO $ do
        -- TODO: figure out if this can be done with knob rather than pipes
        (stdinRead, stdinWrite) <- createPipe
        hSetBuffering stdinRead NoBuffering
        hSetBuffering stdinWrite NoBuffering
        (stdoutRead, stdoutWrite) <- createPipe
        hSetBuffering stdoutRead NoBuffering
        hSetBuffering stdoutWrite NoBuffering
        forkIO $
            forever $ -- remove when LSP stops crashing :-)
                concurrently_
                    (ghcide recorder stdinRead stdoutWrite)
                    (hPut stdinWrite initialize)
        pure (stdinWrite, stdoutRead)
    performEvent $
        onQuery <&> \Query{id, content} -> liftIO $ forkIO $ do
            Text.hPutStr stdin content
            content <- mconcat <$> getAvailableLines stdout
            unless (Text.null content) $ respond FullResponse{id, content}
            respond EndResponse{id}

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
