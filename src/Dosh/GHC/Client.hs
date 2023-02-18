{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.Client where

import Control.Lens
import Control.Monad (forM_, forever)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Catch (SomeException, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Char (isSpace)
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
        onQuery <&> \Query{id, content} -> liftIO $ ghc.input $ do
            let exec = do
                    let statements :: [Text]
                        statements = reverse $ foldl addStatementLine [] $ Text.lines content
                    forM_ statements $ \statement -> GHC.execStmt (Text.unpack statement) GHC.execOptions
                    GHC.execStmt "mapM_ System.IO.hFlush [System.IO.stdout, System.IO.stderr]" GHC.execOptions
            let log = forever $ liftIO $ do
                    content <- Text.hGetLine ghc.output <&> (<> "\n")
                    respond PartialResponse{id, content}
            raceWithDelay_ 1000 exec log `catch` \error -> liftIO (respond Error{id, error})
            liftIO $ respond EndResponse{id}
    pure Client{..}

deriving via (ReaderT Session IO) instance MonadBase IO Ghc

deriving via (ReaderT Session IO) instance MonadBaseControl IO Ghc

addStatementLine :: [Text] -> Text -> [Text]
addStatementLine [] line = [line]
addStatementLine ss "" = ss
addStatementLine (currentStatement : oldStatements) line
    | isSpace (Text.head line) = (currentStatement <> "\n" <> line) : oldStatements
    | otherwise = line : currentStatement : oldStatements

{-
print "foo"
print "bar"
foldr
    (\x acc -> acc + x)
    []
    $ Text.lines foo
-}
