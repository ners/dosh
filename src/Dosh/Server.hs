{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.Server where

import Control.Monad.IO.Class
import Data.Text (Text)
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
    }

data Server t = Server
    { onQuery :: Event t Query
    , respond :: Response -> IO ()
    }

server :: (Reflex t, MonadIO m, TriggerEvent t m) => (Server t -> m a) -> m (Client t)
server handler = do
    (onQuery, query) <- newTriggerEvent
    (onResponse, respond) <- newTriggerEvent
    handler Server{..}
    pure Client{..}
