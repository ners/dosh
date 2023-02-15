{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.GHC.Client where

import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void, (<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Dosh.GHC.Server (Server (..))
import GHC qualified (execOptions, execStmt)
import Reflex hiding (Query, Response)

data Query = Query
    { id :: Int
    , content :: Text
    }

data Response
    = FullResponse {id :: Int, content :: Text}
    | PartialResponse {id :: Int, content :: Text}
    | EndResponse {id :: Int}

data Client t = Client
    { query :: Query -> IO ()
    , onResponse :: Event t Response
    }

client
    :: forall t m
     . ( Reflex t
       , TriggerEvent t m
       , PerformEvent t m
       , MonadIO m
       , MonadIO (Performable m)
       )
    => Server t
    -> m (Client t)
client ghc = do
    (onQuery, query) <- newTriggerEvent @t @m @Query
    (onResponse, respond) <- newTriggerEvent @t @m @Response
    performEvent $
        onQuery <&> \Query{..} -> liftIO $ ghc.input $ do
            void $
                GHC.execStmt (Text.unpack content) GHC.execOptions
                    `finally` liftIO (respond EndResponse{..})
    pure Client{..}
