{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.GHC.Server where

import Control.Monad.Catch (MonadMask, bracket, catch)
import Dosh.GHC.Session qualified as GHC
import Dosh.Prelude hiding (bracket, catch)
import Dosh.Util
import GHC (DynFlags, Ghc, getSessionDynFlags, runGhc)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import GHC.Paths qualified as GHC
import Reflex
    ( Event
    , TriggerEvent (newTriggerEvent)
    )
import System.Process (createPipe)

data Server t = Server
    { input :: Ghc () -> IO ()
    , output :: Handle
    , error :: Handle
    , onError :: Event t SomeException
    }

server
    :: forall t m
     . ( TriggerEvent t m
       , MonadIO m
       )
    => m (Server t)
server = do
    (onError, reportError) <- newTriggerEvent
    (input, output, error) <- liftIO $ asyncServer reportError
    pure Server{..}

asyncServer :: (SomeException -> IO ()) -> IO (Ghc () -> IO (), Handle, Handle)
asyncServer reportError = do
    -- TODO: try to use Knob rather than pipes
    (outRead, outWrite) <- createPipe
    hSetBuffering outRead NoBuffering
    hSetBuffering outWrite NoBuffering
    (errRead, errWrite) <- createPipe
    hSetBuffering errRead NoBuffering
    hSetBuffering errWrite NoBuffering
    sessionActions <- newTQueueIO
    void $ forkIO $ runGhc (Just GHC.libdir) $ do
        GHC.initialiseSession
        forever $ do
            action <- atomically $ readTQueue sessionActions
            hCapture [(stdout, outWrite), (stderr, errWrite)] action
                `catch` (liftIO . reportError)
    let input = atomically . writeTQueue sessionActions
    pure (input, outRead, errRead)

testServer :: Ghc () -> IO (ByteString, ByteString, [SomeException])
testServer action = do
    errors <- newMVar []
    let reportError :: SomeException -> IO ()
        reportError = modifyMVar_ errors . (pure .) . (:)
    (i, o, e) <- asyncServer reportError
    i action
    threadDelay 1_000
    (,,) <$> getAvailableContents o <*> getAvailableContents e <*> takeMVar errors

withGhc :: Ghc a -> IO (Either SomeException a)
withGhc action = try $ runGhc (Just GHC.libdir) $ do
    GHC.initialiseSession
    action

getSessionDynFlags :: Server t -> IO GHC.DynFlags
getSessionDynFlags s = do
    flags <- newEmptyMVar
    s.input $ GHC.getSessionDynFlags >>= putMVar flags
    takeMVar flags

hCapture :: forall m a. (MonadIO m, MonadMask m) => [(Handle, Handle)] -> m a -> m a
hCapture handleMap action = go handleMap
  where
    go :: [(Handle, Handle)] -> m a
    go [] = action
    go ((oldHandle, newHandle) : hs) = do
        buffering <- liftIO $ hGetBuffering oldHandle
        let redirect = liftIO $ do
                old <- hDuplicate oldHandle
                hDuplicateTo newHandle oldHandle
                pure old
            restore old = liftIO $ do
                hDuplicateTo old oldHandle
                hSetBuffering oldHandle buffering
                hClose old
        bracket redirect restore (const $ go hs)

hReset :: Handle -> IO ()
hReset h = hSeek h AbsoluteSeek 0
