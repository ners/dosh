{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.EchoServer where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor
import Data.Text qualified as Text
import Dosh.Server
import Reflex hiding (Query, Response)

echoServer
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
echoServer = server $ \Server{..} ->
    void $
        performEvent $
            onQuery <&> \Query{..} -> liftIO $ forkIO $ do
                forM_ (Text.singleton <$> Text.unpack content) $ \content -> do
                    threadDelay 500_000
                    respond PartialResponse{..}
                respond EndResponse{..}
