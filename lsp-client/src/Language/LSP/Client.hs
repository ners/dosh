{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.LSP.Client where

import Control.Lens
import Control.Monad (forever)
import Data.Aeson (decode)
import Data.ByteString (fromStrict, hGetLine)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Language.LSP.Types
import System.IO (Handle)
import UnliftIO (race)
import Prelude

-- data SessionState = SessionState
--    { sendingQueue :: [Message] -- Probably not needed
--    , requestsWaitingForResponse :: Map (LspId, LspRequest) (LspRequest -> LspResponse -> IO ())
--    , lastDiagnostics :: Maybe [Diagnostics]
--    }
--
class HasDiagnostics a where
    diagnostics :: Lens' a (Maybe [Diagnostic])

--
type Session = IO

data ServerResponse

--
-- runSession :: Session a -> IO a
-- runSession = runSessionWithHandles stdin stdout

runSessionWithHandles :: Handle -> Handle -> Session a -> IO a
runSessionWithHandles input output action = do
    actionResult <- race action $ forever $ do
        serverMessage <- fromJust . decode . fromStrict <$> hGetLine input
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
