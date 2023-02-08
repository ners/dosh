{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.Server where

import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Monad.IO.Class
import Reflex hiding (Query, Response)
import Control.Monad.Fix
import qualified Data.Text as Text
import Control.Lens
import Control.Concurrent
import Control.Monad

data Query = Query
    { id :: Int
    , content :: Text
    }
    deriving stock (Generic)

data Response
    = PartialResponse
        {id :: Int, content :: Text}
    | FullResponse
        {id :: Int, content :: Text}
    | EndResponse
        {id :: Int}
    deriving stock (Generic)

data Server t = Server
    { query :: Query -> IO ()
    , response :: Event t Response
    }

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
    => m (Server t)
echoServer = do
    (onQuery, query) <- newTriggerEvent
    (response, triggerResponse) <- newTriggerEvent
    performEvent $ onQuery <&> \Query {id, content} -> liftIO $ forkIO $ do
            forM_ (Text.singleton <$> Text.unpack content) $ \content -> do
                threadDelay 500_000
                triggerResponse PartialResponse{..}
            triggerResponse EndResponse{..}
    pure Server{..}
