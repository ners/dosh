{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.LSP.Client.Decoding where

import Control.Exception (catch, throw)
import Data.Aeson (Result (Error, Success), Value, decode)
import Data.Aeson.Types (parse)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.Functor
import Data.Functor.Const
import Data.Functor.Product (Product (Pair))
import Data.IxMap (IxMap, emptyIxMap, insertIxMap, pickFromIxMap)
import Data.Maybe (fromJust)
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

-- type RequestMap = IxMap LspId (SMethod :: Method 'FromClient 'Request -> Type)
--
type RequestMap = IxMap LspId Callback

-- data RequestMap where
--  RequestMap :: IxMap LspId Callback -> RequestMap

data Callback (m :: Method 'FromClient 'Request) = Callback
    { callback :: ResponseMessage m -> IO ()
    , method :: SMethod m
    }

newRequestMap :: RequestMap
newRequestMap = emptyIxMap

updateRequestMap :: RequestMap -> LspId m -> Callback m -> Maybe RequestMap
updateRequestMap reqMap id callback = insertIxMap id callback reqMap

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

decodeFromServerMsg :: RequestMap -> LazyByteString -> (RequestMap, FromServerMessage, IO ())
decodeFromServerMsg reqMap bytes = unP $ parse p obj
  where
    obj = fromJust $ decode bytes :: Value
    p = parseServerMessage $ \(lid :: LspId m) ->
        let (maybeCallback, newMap) = pickFromIxMap lid reqMap
         in maybeCallback <&> \c -> (c.method, Pair c (Const newMap))
    -- case maybeCallback of
    --       Nothing -> Nothing
    --       Just m -> Just (m, Pair m (Const newMap))
    unP (Success (FromServerMess m msg)) = (reqMap, FromServerMess m msg, pure ())
    unP (Success (FromServerRsp (Pair c (Const newMap)) msg)) = (newMap, FromServerRsp c.method msg, c.callback msg)
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
