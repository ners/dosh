{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.Client where

import Control.Lens
import Control.Monad (forM_, forever, (>=>))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Catch (SomeException, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.ByteString (ByteString, hGetSome)
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Char (isSpace)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Generics.Product (position)
import Data.List (stripPrefix)
import Data.List.Lens (stripSuffix)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID (UUID)
import Dosh.GHC.Server (Server (..))
import Dosh.GHC.Session as GHC
import Dosh.Util
import GHC qualified
import GHC.Driver.Monad (Ghc (..), Session (..))
import GHC.Generics (Generic)
import GHC.Utils.Misc (split)
import Reflex hiding (Query, Response)

data Query = Query
    { uid :: UUID
    , content :: Text
    }

data Response
    = FullResponse {uid :: UUID, content :: ByteString}
    | PartialResponse {uid :: UUID, content :: ByteString}
    | Error {uid :: UUID, error :: SomeException}
    | EndResponse {uid :: UUID}

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
                    forM_ (splitCommands content) $ \case
                        Import lib -> GHC.addImport lib
                        LanguageExtensions (split ',' -> exts) -> forM_ exts GHC.applyExtensionString
                        Statement s -> void $ GHC.execStmt s GHC.execOptions
                    GHC.execStmt "mapM_ hFlush [stdout, stderr]" GHC.execOptions
            let log = forever $ liftIO $ do
                    content <- hGetSome ghc.output defaultChunkSize
                    respond PartialResponse{..}
            raceWithDelay_ 1000 exec log `catch` \error -> liftIO (respond Error{..})
            liftIO $ respond EndResponse{..}
    pure Client{..}

deriving via (ReaderT Session IO) instance MonadBase IO Ghc

deriving via (ReaderT Session IO) instance MonadBaseControl IO Ghc

data Command
    = Import String
    | LanguageExtensions String
    | Statement String
    deriving stock (Generic, Show, Eq)

instance IsString Command where
    fromString (stripPrefix "import " -> Just lib) = Import lib
    fromString (stripPrefix "{-# LANGUAGE " >=> stripSuffix " #-}" -> Just lang) = LanguageExtensions lang
    fromString s = Statement s

{- | Splits a text object into a list of commands to be evaluated.
 Each line of the input is a new command, unless it starts with a whitespace character,
 in which case it is appended to the previous command.
-}
splitCommands :: Text -> [Command]
splitCommands = reverse . foldl addCommandLine [] . Text.lines

addCommandLine :: [Command] -> Text -> [Command]
addCommandLine [] line = [fromText line]
addCommandLine ss "" = ss
addCommandLine (currentStatement : oldStatements) line
    | isSpace (Text.head line) = (currentStatement `append` "\n" `append` line) : oldStatements
    | otherwise = fromText line : currentStatement : oldStatements
  where
    append :: Command -> Text -> Command
    append c t = c & position @1 %~ (<> Text.unpack t)
