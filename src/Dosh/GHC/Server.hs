{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.GHC.Server where

import Control.Arrow ((>>>))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Control.Monad.Catch (MonadMask, SomeException, bracket, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Dosh.GHC.Session qualified as GHC
import GHC (Ghc, runGhc)
import GHC.IO.Handle (BufferMode (NoBuffering), hDuplicate, hDuplicateTo)
import GHC.Paths qualified as GHC
import Reflex
    ( Event
    , Reflex
    , TriggerEvent (newTriggerEvent)
    )
import System.IO
    ( Handle
    , SeekMode (AbsoluteSeek)
    , hClose
    , hGetBuffering
    , hSeek
    , hSetBuffering
    , stderr
    , stdout
    )
import System.Process (createPipe)
import Prelude

data Server t = Server
    { input :: Ghc () -> IO ()
    , output :: Handle
    , error :: Handle
    , onError :: Event t SomeException
    }

server
    :: forall t m
     . ( Reflex t
       , TriggerEvent t m
       , MonadIO m
       )
    => m (Server t)
server = do
    (onError, reportError) <- newTriggerEvent
    i <- liftIO newEmptyMVar
    o <- liftIO newEmptyMVar
    (output, error) <- liftIO $ do
        -- TODO: try to use Knob rather than pipes
        (outRead, outWrite) <- createPipe
        hSetBuffering outRead NoBuffering
        hSetBuffering outWrite NoBuffering
        (errRead, errWrite) <- createPipe
        hSetBuffering errRead NoBuffering
        hSetBuffering errWrite NoBuffering
        forkIO $ runGhc (Just GHC.libdir) $ do
            GHC.initialiseSession
            forever $ do
                a <- liftIO $ takeMVar i
                result <-
                    hCapture [(stdout, outWrite), (stderr, errWrite)] a
                        `catch` (liftIO . reportError)
                liftIO $ putMVar o result
        pure (outRead, errRead)
    pure
        Server
            { input = putMVar i >>> (*> takeMVar o)
            , output
            , error
            , onError
            }

-- instance PerformEvent t m => PerformEvent t (GhcT m) where
--    type Performable (GhcT m) = Performable m
--    performEvent = liftGhcT . performEvent
--    performEvent_ = liftGhcT . performEvent_

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
