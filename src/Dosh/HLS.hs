{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.HLS where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Dosh.Server
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
hls = do
    server $ \Server{..} ->
        void $
            performEvent $
                onQuery <&> \Query{..} -> liftIO $ do
                    respond EndResponse{..}
