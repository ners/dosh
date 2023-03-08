{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.LSP.Client where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (decode)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import GHC.Conc (writeTVar)
import Language.LSP.Client.Decoding
    ( RequestMap
    , decodeFromServerMsg
    , getNextMessage
    , newRequestMap
    )
import Language.LSP.Types
import Language.LSP.Types (FromClientMessage' (FromClientRsp), FromServerMessage, FromServerMessage' (FromServerMess))
import System.IO (Handle, stdin, stdout)
import UnliftIO (liftIO, race)
import UnliftIO.STM
import Prelude

data SessionState = SessionState
    { sendingQueue :: ()
    , -- [Message] -- Probably not needed
      pendingRequests :: RequestMap
    , -- Map (LspId, LspRequest) (LspRequest -> LspResponse -> IO ())
      lastDiagnostics :: Maybe [Diagnostic]
    }

defaultSessionState :: SessionState
defaultSessionState =
    SessionState
        { sendingQueue = ()
        , pendingRequests = newRequestMap
        , lastDiagnostics = Nothing
        }

class HasDiagnostics a where
    diagnostics :: Lens' a (Maybe [Diagnostic])

type Session = ReaderT (TVar SessionState) IO

data ServerResponse

runSession :: Session a -> IO a
runSession = runSessionWithHandles stdin stdout

runSessionWithHandles :: Handle -> Handle -> Session a -> IO a
runSessionWithHandles input output action = do
    initialState <- newTVarIO defaultSessionState
    flip runReaderT initialState $ do
        actionResult <- race action $ forever $ do
            serverBytes <- liftIO $ getNextMessage input
            sessionState <- ask >>= readTVarIO
            let (requestMap, serverMessage) = decodeFromServerMsg (pendingRequests sessionState) serverBytes
            let newState = sessionState{pendingRequests = requestMap}
            ask >>= (atomically . flip writeTVar newState)
            case serverMessage of
                FromServerMess _ _ -> pure () -- if diagnostics, update state
                FromServerRsp _ _ -> pure () -- assign response to request
        pure $ fromRight undefined actionResult

sendRequest
    :: SClientMethod m
    -> MessageParams m
    -> (LspId m -> ResponseMessage m -> IO ())
    -> Session (LspId m)
sendRequest method params callback = undefined

--
-- sendNotification :: LspNotification -> Session ()
-- sendNotification notification = undefined
--
getDiagnostics :: Session [Diagnostic]
getDiagnostics = undefined

--
-- getDocumentContents :: Uri -> Session Text
-- getDocumentContents uri = undefined
