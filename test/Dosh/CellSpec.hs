module Dosh.CellSpec (spec) where

import Control.Concurrent
import Control.Concurrent.Async (race)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Dosh.Cell
import Dosh.Server (echoServer)
import Dosh.Util
import Graphics.Vty hiding (Event)
import Reflex
import Reflex.Vty
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Cell" $ do
    evalOnEnterSpec

-- This datatype signals that the FRP network was able to exit on its own.
data Exit = Exit deriving (Show, Eq)

-- This creates the MVar with which the FRP network sends the exit signals, and
-- checks if a response from the FRP networks comes back in the allotted time.
timeoutWrapperAsync :: Int -> (MVar Exit -> IO ()) -> IO (Either () (Maybe Exit))
timeoutWrapperAsync us wrapped = do
    exitCommMVar :: MVar Exit <- newEmptyMVar
    race (wrapped exitCommMVar) (timeout us $ takeMVar exitCommMVar)

withTimeout :: Int -> (MVar Exit -> IO ()) -> Expectation
withTimeout us wrapped = do
    result <- timeoutWrapperAsync us wrapped
    case result of
        Right (Just Exit) -> pure ()
        Right Nothing -> expectationFailure "Application did not exit in time"
        _ -> expectationFailure "Application did not exit successfully"

seconds :: Num a => a -> a
seconds = (1_000 *) . milliseconds

milliseconds :: Num a => a -> a
milliseconds = (1_000 *)

evalOnEnterSpec :: Spec
evalOnEnterSpec = it "evaluates user input on enter" $ withTimeout (seconds 1) $ \exitMVar -> do
    putMVar exitMVar Exit
    threadDelay (seconds 1)
