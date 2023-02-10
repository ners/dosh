{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.Server where

import Control.Monad.IO.Class
import Data.Text (Text)
import Development.IDE.Main (Log)
import Development.IDE.Types.Logger (WithPriority)
import GHC.Generics (Generic)
import Reflex hiding (Query, Response)

data Query = Query
    { id :: Int
    , content :: Text
    }
    deriving stock (Generic, Eq, Show)

data Response
    = PartialResponse
        {id :: Int, content :: Text}
    | FullResponse
        {id :: Int, content :: Text}
    | EndResponse
        {id :: Int}
    deriving stock (Generic, Eq, Show)

data Client t = Client
    { query :: Query -> IO ()
    , onResponse :: Event t Response
    , onLog :: Event t (WithPriority Log)
    }

data Server t = Server
    { onQuery :: Event t Query
    , respond :: Response -> IO ()
    , log :: WithPriority Log -> IO ()
    }

server :: (Reflex t, MonadIO m, TriggerEvent t m) => (Server t -> m a) -> m (Client t)
server handler = do
    (onQuery, query) <- newTriggerEvent
    (onResponse, respond) <- newTriggerEvent
    (onLog, log) <- newTriggerEvent
    handler Server{..}
    pure Client{..}
