module Dosh.LSP.Session where

import Language.LSP.Client (runSessionWithHandles)
import Language.LSP.Client.Session (Session)
import Prelude

runSession :: Session a -> IO a
runSession actions = do
    let serverIn = undefined
        serverOut = undefined
    runSessionWithHandles serverOut serverIn do
        actions
