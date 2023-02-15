{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.Client where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Catch (SomeException, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Dosh.GHC.Server (Server (..))
import Dosh.Util
import GHC qualified (execOptions, execStmt)
import GHC.Driver.Monad (Ghc (..), Session (..))
import Reflex hiding (Query, Response)

data Query = Query
    { id :: Int
    , content :: Text
    }

data Response
    = FullResponse {id :: Int, content :: Text}
    | PartialResponse {id :: Int, content :: Text}
    | Error {id :: Int, error :: SomeException}
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
    (onQuery, query) <- newTriggerEvent
    (onResponse, respond) <- newTriggerEvent
    performEvent $
        onQuery <&> \Query{..} -> liftIO $ ghc.input $ do
            let exec = do
                    result <- GHC.execStmt (Text.unpack content) GHC.execOptions
                    GHC.execStmt "System.IO.hFlush System.IO.stdout" GHC.execOptions
                    pure result
            let log = forever $ liftIO $ do
                    content <- Text.hGetLine ghc.output <&> (<> "\n")
                    respond PartialResponse{..}
            raceWithDelay_ 50 exec log `catch` \error -> liftIO (respond Error{..})
            liftIO $ respond EndResponse{..}
    pure Client{..}

deriving via (ReaderT Session IO) instance MonadBase IO Ghc

deriving via (ReaderT Session IO) instance MonadBaseControl IO Ghc
