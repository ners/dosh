{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Dosh.GHC.Client where

import Control.Monad.Reader (ReaderT (..))
import Data.ByteString (hGetSome)
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Generics.Labels ()
import Data.UUID (UUID)
import Dosh.GHC.Evaluator qualified as GHC
import Dosh.GHC.Server (Server (..))
import Dosh.Prelude
import Dosh.Util
import GHC.Driver.Monad (Ghc (..), Session (..))
import Reflex hiding (Request, Response)

data Request = Evaluate {uid :: UUID, content :: Text}

data Response
    = FullResponse {uid :: UUID, content :: ByteString}
    | PartialResponse {uid :: UUID, content :: ByteString}
    | Error {uid :: UUID, error :: SomeException}
    | EndResponse {uid :: UUID}

data Client t = Client
    { request :: Request -> IO ()
    , onResponse :: Event t Response
    }

client
    :: forall t m
     . ( TriggerEvent t m
       , PerformEvent t m
       , MonadIO (Performable m)
       )
    => Server t
    -> m (Client t)
client ghc = do
    (onRequest, request) <- newTriggerEvent
    (onResponse, respond) <- newTriggerEvent
    performEvent $
        onRequest <&> \case
            Evaluate{..} -> liftIO $ ghc.input $ do
                let exec = do
                        GHC.evaluate content
                        GHC.evaluate "mapM_ hFlush [stdout, stderr]"
                let log = forever $ liftIO $ do
                        content <- hGetSome ghc.output defaultChunkSize
                        respond PartialResponse{..}
                raceWithDelay_ 1000 exec log `catch` \error -> liftIO (respond Error{..})
                liftIO $ respond EndResponse{..}
    pure Client{..}

deriving via (ReaderT Session IO) instance MonadUnliftIO Ghc
