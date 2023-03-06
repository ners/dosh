module Language.LSP.Client where

data SessionState = SessionState
    { sendingQueue :: [Message] -- Probably not needed
    , requestsWaitingForResponse :: Map (LspId, LspRequest) (LspRequest -> LspResponse -> IO ())
    , lastDiagnostics :: Maybe [Diagnostics]
    }

class HasDiagnostics a where
    diagnostics :: Lens' a (Maybe [Diagnostics])

type Session = IO

runSession :: Session a -> IO a
runSession = runSessionWithHandles stdin stdout

runSessionWithHandles :: Handle -> Handle -> Session a -> IO a
runSessionWithHandles input output action = race action $ forever $ do
    serverMessage <- fromJson <$> hGetLine input
    if isDiagnostics serverMessage
        then --update state to save lastDiagnostics
        else pure ()

sendRequest :: LspRequest -> (LspRequest -> LspResponse -> IO ()) -> Session LspId
sendRequest request callback = undefined

sendNotification :: LspNotification -> Session ()
sendNotification notification = undefined

getDiagnostics :: HasDiagnostics m => m [Diagnostics]
getDiagnostics = undefined

getDocumentContents :: Uri -> Session Text
getDocumentContents uri = undefined
