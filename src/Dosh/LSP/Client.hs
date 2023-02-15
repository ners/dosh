{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.LSP.Client where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import Data.Text (Text)
import Dosh.LSP.Server (Server (..))
import Dosh.Util
import Language.LSP.Test (Session (..), defaultConfig, fullCaps, request, runSessionWithHandles)
import Language.LSP.Types
import Reflex
    ( Event
    , MonadHold
    , PerformEvent
    , PostBuild
    , Reflex
    , TriggerEvent
    , newTriggerEvent
    )
import Reflex.PerformEvent.Class (PerformEvent (..))
import Prelude hiding (id)

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
       , MonadIO m
       , PerformEvent t m
       , TriggerEvent t m
       , MonadIO (Performable m)
       , PostBuild t m
       , MonadFix m
       , MonadHold t m
       , PerformEvent t Session
       , MonadIO (Performable m)
       )
    => Server t
    -> m (Client t)
client server = do
    (onQuery, query) <- newTriggerEvent
    (onResponse, respond) <- newTriggerEvent
    -- TODO: try to `performEvent` inside `runSessionWithHandles` to get rid of `qvar`
    qvar <- liftIO newEmptyMVar
    liftIO $ forkIO $ runSessionWithHandles server.input server.output defaultConfig fullCaps "" $ forever $ do
        Query{..} <- liftIO $ takeMVar qvar
        response <- request STextDocumentCodeLens (CodeLensParams Nothing Nothing $ TextDocumentIdentifier $ Uri "")
        liftIO $ do
            respond FullResponse{id, content = tshow response._result}
            respond EndResponse{id}
    performEvent $ liftIO . putMVar qvar <$> onQuery
    pure Client{..}

-- instance MonadRef Session where
--    type Ref Session = Ref IO
--    newRef = liftIO . newRef
--    readRef = liftIO . readRef
--    writeRef r a = liftIO $ writeRef r a
