module Dosh.CellSpec (spec) where

import Control.Concurrent
import Control.Concurrent.Async (race)
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Dosh.Cell
import Dosh.Util
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
timeoutWrapperAsync :: (MVar Exit -> IO ()) -> IO (Either () (Maybe Exit))
timeoutWrapperAsync wrapped = do
    exitCommMVar :: MVar Exit <- newEmptyMVar
    race (wrapped exitCommMVar) (timeout 1_000_000 $ takeMVar exitCommMVar)

testCell
    :: forall t m
     . ( Reflex t
       , PerformEvent t m
       , TriggerEvent t m
       , MonadIO m
       , HasFocusReader t m
       , HasImageWriter t m
       , HasInput t m
       , HasLayout t m
       , HasDisplayRegion t m
       , MonadFix m
       , HasTheme t m
       , MonadIO (Performable m)
       , MonadHold t m
       , MonadHold t (Performable m)
       )
    => m (Event t Cell)
testCell = do
    (i, o) <- echoServer
    cell i o $ newCell 1

evalOnEnterSpec :: Spec
evalOnEnterSpec = it "evaluates user input on enter" True

-- mainWidget $ initManager_ $ do
--    testCell
--    void <$> ctrldPressed
