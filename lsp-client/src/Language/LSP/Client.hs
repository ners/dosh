{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.LSP.Client where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (decode)
import Data.ByteString (fromStrict, hGetLine)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Language.LSP.Types
import System.IO (Handle, stdin, stdout)
import UnliftIO (liftIO, race)
import UnliftIO.STM
import Prelude

data SessionState = SessionState
    { sendingQueue :: ()
    , -- [Message] -- Probably not needed
      requestsWaitingForResponse :: ()
    , -- Map (LspId, LspRequest) (LspRequest -> LspResponse -> IO ())
      lastDiagnostics :: Maybe [Diagnostic]
    }

defaultSessionState :: SessionState
defaultSessionState =
    SessionState
        { sendingQueue = ()
        , requestsWaitingForResponse = ()
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
            serverMessage <- fromJust . decode . fromStrict <$> liftIO (hGetLine input)
            if isDiagnostics serverMessage
                then pure () -- update state to save lastDiagnostics
                else pure ()
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

isDiagnostics :: ServerMessage m -> Bool
isDiagnostics = undefined
