{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}

{- | External reference with reactivity support. The reference is needed in glue
 code between reflex and external libs where you cannot sample from dynamics
 with `MonadSample`.
 Adapted from: https://github.com/hexresearch/ergvein/tree/master/reflex-external-ref
-}
module Reflex.ExternalRef
    ( ExternalRef (..)
    , newExternalRef
    , readExternalRef
    , writeExternalRef
    , modifyExternalRef
    , modifyExternalRef_
    , externalRefEvent
    , externalRefBehavior
    , externalRefDynamic
    )
where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad.IO.Class
import GHC.Generics
import Reflex

{- | Holds value of type `a` and provides ways for notifying FRP network about
 changes of the variable.

 This abstraction is helpful for storing counters, lists of internal resources
 and so on. It is designed to be updated from outputs of FRP network, not from
 outer world.
-}
data ExternalRef t a = ExternalRef
    { externalRef :: !(MVar a)
    -- ^ Storage of value (do not change this by yourself, use helpers)
    , externalEvent :: !(Event t a)
    -- ^ Event that fires when value is changed
    , externalFire :: !(a -> IO ())
    -- ^ Method of updating value of previous event
    }
    deriving (Generic)

instance NFData (ExternalRef t a) where
    rnf ExternalRef{..} =
        externalRef `deepseq`
            externalEvent `seq`
                externalFire `seq`
                    ()

-- | Creation of external ref in host monad
newExternalRef :: (MonadIO m, TriggerEvent t m) => m (ExternalRef t a)
newExternalRef = do
    externalRef <- liftIO newEmptyMVar
    (externalEvent, externalFire) <- newTriggerEvent
    pure ExternalRef{..}

-- | Read current value of external reference
readExternalRef :: MonadIO m => ExternalRef t a -> m a
readExternalRef = liftIO . takeMVar . externalRef

{- | Write new value to external reference and notify FRP network.
 The function evaluates the value to WNF.
-}
writeExternalRef :: MonadIO m => ExternalRef t a -> a -> m ()
writeExternalRef ExternalRef{..} a = do
    a `seq` liftIO (putMVar externalRef a)
    _ <- liftIO $ externalFire a
    return ()

{- | Atomically modify an external ref and notify FRP network.
 The function evaluates the value to WNF.
-}
modifyExternalRef :: MonadIO m => ExternalRef t a -> (a -> IO (a, b)) -> m b
modifyExternalRef ExternalRef{..} f = liftIO $ modifyMVar externalRef $ \a -> do
    (a', b) <- f a
    externalFire a'
    pure (a', b)

{- | Atomically modify an external ref and notify FRP network.
 The function evaluates the value to WNF. Returns nothing
-}
modifyExternalRef_ :: MonadIO m => ExternalRef t a -> (a -> IO a) -> m ()
modifyExternalRef_ ExternalRef{..} f = liftIO $ modifyMVar_ externalRef $ \a -> do
    a' <- f a
    externalFire a'
    pure a'

-- | Get dynamic that tracks value of the internal ref
externalRefEvent :: (MonadHold t m, MonadIO m) => ExternalRef t a -> m (Event t a)
externalRefEvent = pure . externalEvent

-- | Construct a behavior from external reference
externalRefBehavior :: (MonadHold t m, MonadIO m) => ExternalRef t a -> m (Behavior t a)
externalRefBehavior ExternalRef{..} = do
    a <- liftIO $ takeMVar externalRef
    hold a externalEvent

-- | Get dynamic that tracks value of the internal ref
externalRefDynamic :: (MonadHold t m, MonadIO m) => ExternalRef t a -> m (Dynamic t a)
externalRefDynamic ExternalRef{..} = do
    a <- liftIO $ takeMVar externalRef
    holdDyn a externalEvent
