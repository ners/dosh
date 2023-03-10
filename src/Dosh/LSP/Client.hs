{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Dosh.LSP.Client where

import Dosh.LSP.Server (Server (..))
import Dosh.Prelude
import Dosh.Util
import Language.LSP.Test (request)
import Language.LSP.Types
import Reflex hiding (Query, Response)
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
     . ( PerformEvent t m
       , TriggerEvent t m
       , MonadUnliftIO (Performable m)
       )
    => Server t
    -> m (Client t)
client server = do
    (onQuery, query) <- newTriggerEvent
    (onResponse, respond) <- newTriggerEvent
    performEvent $
        onQuery <&> \Query{..} -> liftIO $ server.input $ do
            response <- request STextDocumentCodeLens (CodeLensParams Nothing Nothing $ TextDocumentIdentifier $ Uri "")
            liftIO $ do
                respond FullResponse{id, content = tshow response._result}
                respond EndResponse{id}
    pure Client{..}
