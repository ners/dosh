{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Client.Decoding where

import Control.Exception (catch, throw)
import Control.Monad (liftM2)
import Data.Aeson (Result (Error, Success), Value, decode)
import Data.Aeson.Types (parse)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Functor
import Data.Functor.Const
import Data.Functor.Product (Product (Pair))
import Data.IxMap (IxMap)
import Data.IxMap qualified as IxMap
import Data.Maybe (fromJust, fromMaybe)
import Language.LSP.Client.Exceptions
import Language.LSP.Types
import System.IO (Handle, hGetLine)
import System.IO.Error (isEOFError)
import Prelude hiding (id)

{- | Fetches the next message bytes based on
 the Content-Length header
-}
getNextMessage :: Handle -> IO LazyByteString
getNextMessage h = do
    headers <- getHeaders h
    case read . init <$> lookup "Content-Length" headers of
        Nothing -> throw NoContentLengthHeader
        Just size -> LazyByteString.hGet h size

getHeaders :: Handle -> IO [(String, String)]
getHeaders h = do
    l <- catch (hGetLine h) eofHandler
    let (name, val) = span (/= ':') l
    if null val then return [] else ((name, drop 2 val) :) <$> getHeaders h
  where
    eofHandler :: IOError -> a
    eofHandler e
        | isEOFError e = throw UnexpectedServerTermination
        | otherwise = throw e

type RequestMap = IxMap LspId RequestCallback

emptyRequestMap :: RequestMap
emptyRequestMap = IxMap.emptyIxMap

data RequestCallback (m :: Method 'FromClient 'Request) = RequestCallback
    { requestCallback :: ResponseMessage m -> IO ()
    , requestMethod :: SMethod m
    }

type NotificationMap = DMap SMethod NotificationCallback

emptyNotificationMap :: NotificationMap
emptyNotificationMap = mempty

newtype NotificationCallback (m :: Method 'FromServer 'Notification) = NotificationCallback
    { notificationCallback :: Message m -> IO ()
    }

instance Semigroup (NotificationCallback m) where
    (NotificationCallback c1) <> (NotificationCallback c2) = NotificationCallback $ liftM2 (*>) c1 c2

instance Monoid (NotificationCallback m) where
    mempty = NotificationCallback (const $ pure ())

updateRequestMap :: LspId m -> RequestCallback m -> RequestMap -> RequestMap
updateRequestMap = ((fromMaybe (error "updateRequestMap: duplicate key registration") .) .) . IxMap.insertIxMap

updateNotificationMap :: SMethod m -> NotificationCallback m -> NotificationMap -> NotificationMap
updateNotificationMap = DMap.insertWith' (<>)

-- getRequestMap :: [FromClientMessage] -> RequestMap
-- getRequestMap = foldl' helper emptyIxMap
--  where
--    helper :: RequestMap -> FromClientMessage -> RequestMap
--    helper acc msg = case msg of
--        FromClientMess m mess -> case splitClientMethod m of
--            IsClientNot -> acc
--            IsClientReq -> fromJust $ updateRequestMap acc (mess ^. id) m
--            IsClientEither -> case mess of
--                NotMess _ -> acc
--                ReqMess msg -> fromJust $ updateRequestMap acc (msg ^. id) m
--        _ -> acc

decodeFromServerMsg :: LazyByteString -> RequestMap -> ((FromServerMessage, IO ()), RequestMap)
decodeFromServerMsg bytes reqMap = unP $ parse p obj
  where
    obj = fromJust $ decode bytes :: Value
    p = parseServerMessage $ \(lid :: LspId m) ->
        let (maybeCallback, newMap) = IxMap.pickFromIxMap lid reqMap
         in maybeCallback <&> \c -> (c.requestMethod, Pair c (Const newMap))
    -- case maybeCallback of
    --       Nothing -> Nothing
    --       Just m -> Just (m, Pair m (Const newMap))
    unP (Success (FromServerMess m msg)) = ((FromServerMess m msg, pure ()), reqMap)
    unP (Success (FromServerRsp (Pair c (Const newMap)) msg)) = ((FromServerRsp c.requestMethod msg, c.requestCallback msg), newMap)
    unP (Error e) = error $ "Error decoding " <> show obj <> " :" <> e

{-
  WorkspaceWorkspaceFolders      -> error "ReqWorkspaceFolders not supported yet"
  WorkspaceConfiguration         -> error "ReqWorkspaceConfiguration not supported yet"
  CustomServerMethod _
      | "id" `HM.member` obj && "method" `HM.member` obj -> ReqCustomServer $ fromJust $ decode bytes
      | "id" `HM.member` obj -> RspCustomServer $ fromJust $ decode bytes
      | otherwise -> NotCustomServer $ fromJust $ decode bytes

Error e -> error e
-}
