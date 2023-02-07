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
    , modifyExternalRefMaybe
    , modifyExternalRefMaybe_
    , modifyExternalRefM
    , modifyExternalRefM_
    , modifyExternalRefMaybeM
    , modifyExternalRefMaybeM_
    , externalRefBehavior
    , externalRefDynamic
    , externalFromDynamic
    , fmapExternalRef
    )
where

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.IORef
import GHC.Generics
import Reflex

{- | Holds value of type `a` and provides ways for notifying FRP network about
 changes of the variable.

 This abstraction is helpful for storing counters, lists of internal resources
 and so on. It is designed to be updated from outputs of FRP network, not from
 outer world.
-}
data ExternalRef t a = ExternalRef
    { externalRef :: !(IORef a)
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
newExternalRef :: (MonadIO m, TriggerEvent t m) => a -> m (ExternalRef t a)
newExternalRef a = do
    ref <- liftIO $ newIORef a
    (e, fire) <- newTriggerEvent
    return $ ExternalRef ref e fire

-- | Read current value of external reference
readExternalRef :: MonadIO m => ExternalRef t a -> m a
readExternalRef ExternalRef{..} = liftIO $ readIORef externalRef

{- | Write new value to external reference and notify FRP network.
 The function evaluates the value to WNF.
-}
writeExternalRef :: MonadIO m => ExternalRef t a -> a -> m ()
writeExternalRef ExternalRef{..} a = do
    a `seq` liftIO (writeIORef externalRef a)
    _ <- liftIO $ externalFire a
    return ()

{- | Atomically modify an external ref and notify FRP network.
 The function evaluates the value to WNF.
-}
modifyExternalRef :: MonadIO m => ExternalRef t a -> (a -> (a, b)) -> m b
modifyExternalRef ExternalRef{..} f = do
    (a, b) <- liftIO $ atomicModifyIORef' externalRef $ \a ->
        let (a', b) = f a in (a', (a', b))
    _ <- liftIO $ externalFire a
    return b

{- | Atomically modify an external ref and notify FRP network.
 The function evaluates the value to WNF. Returns nothing
-}
modifyExternalRef_ :: MonadIO m => ExternalRef t a -> (a -> a) -> m ()
modifyExternalRef_ ExternalRef{..} f = do
    a <- liftIO $ atomicModifyIORef' externalRef $ \a ->
        let a' = f a in (a', a')
    liftIO $ externalFire a

{- | If the function evaluates to Just then
 Atomically modify an external ref and notify FRP network.
 The function evaluates the value to WNF.
 Return the Maybe result of function's evaluation
-}
modifyExternalRefMaybe :: MonadIO m => ExternalRef t a -> (a -> Maybe (a, b)) -> m (Maybe b)
modifyExternalRefMaybe ExternalRef{..} f = do
    mab <- liftIO $ atomicModifyIORef' externalRef $ \a ->
        maybe (a, Nothing) (\ab -> (fst ab, Just ab)) $ f a
    liftIO $ maybe (pure ()) (externalFire . fst) mab
    pure $ snd <$> mab

{- | If the function evaluates to Just then
 Atomically modify an external ref and notify FRP network.
 The function evaluates the value to WNF. Returns nothing
 The function discards the result
-}
modifyExternalRefMaybe_ :: MonadIO m => ExternalRef t a -> (a -> Maybe a) -> m ()
modifyExternalRefMaybe_ ExternalRef{..} f = do
    ma <- liftIO $ atomicModifyIORef' externalRef $ \a ->
        maybe (a, Nothing) (\a' -> (a', Just a')) $ f a
    liftIO $ maybe (pure ()) externalFire ma

{- | Modify (not atomically) an external ref and notify FRP network.
 The function evaluates the value to WNF.
-}
modifyExternalRefM :: MonadIO m => ExternalRef t a -> (a -> m (a, b)) -> m b
modifyExternalRefM ExternalRef{..} f = do
    a <- liftIO $ readIORef externalRef
    (a', b) <- f a
    liftIO $ do
        writeIORef externalRef a'
        externalFire a'
    return b

{- | Modify (not atomically) an external ref and notify FRP network.
 The function evaluates the value to WNF.
-}
modifyExternalRefM_ :: MonadIO m => ExternalRef t a -> (a -> m a) -> m ()
modifyExternalRefM_ ExternalRef{..} f = do
    a <- liftIO $ readIORef externalRef
    a' <- f a
    liftIO $ do
        writeIORef externalRef a'
        externalFire a'

{- | If the function evaluates to Just then
 Modify (not atomically) an external ref and notify FRP network.
 The function evaluates the value to WNF.
-}
modifyExternalRefMaybeM :: MonadIO m => ExternalRef t a -> (a -> m (Maybe (a, b))) -> m (Maybe b)
modifyExternalRefMaybeM ExternalRef{..} f = do
    a <- liftIO $ readIORef externalRef
    mab <- f a
    case mab of
        Nothing -> pure Nothing
        Just (a', b) -> liftIO $ do
            writeIORef externalRef a'
            externalFire a'
            return $ Just b

{- | If the function evaluates to Just then
 Modify (not atomically) an external ref and notify FRP network.
 The function evaluates the value to WNF.
 The function discards the result
-}
modifyExternalRefMaybeM_ :: MonadIO m => ExternalRef t a -> (a -> m (Maybe a)) -> m ()
modifyExternalRefMaybeM_ ExternalRef{..} f = do
    a <- liftIO $ readIORef externalRef
    ma <- f a
    case ma of
        Nothing -> pure ()
        Just a' -> liftIO $ do
            writeIORef externalRef a'
            externalFire a'

-- | Construct a behavior from external reference
externalRefBehavior :: (MonadHold t m, MonadIO m) => ExternalRef t a -> m (Behavior t a)
externalRefBehavior ExternalRef{..} = do
    a <- liftIO $ readIORef externalRef
    hold a externalEvent

-- | Get dynamic that tracks value of the internal ref
externalRefDynamic :: (MonadHold t m, MonadIO m) => ExternalRef t a -> m (Dynamic t a)
externalRefDynamic ExternalRef{..} = do
    a <- liftIO $ readIORef externalRef
    holdDyn a externalEvent

{- | Create external ref that tracks content of dynamic. Editing of the ref
 has no effect on the original dynamic.
-}
externalFromDynamic
    :: (MonadHold t m, TriggerEvent t m, PerformEvent t m, Reflex t, MonadIO m, MonadIO (Performable m))
    => Dynamic t a
    -> m (ExternalRef t a)
externalFromDynamic da = do
    a0 <- sample . current $ da
    r <- newExternalRef a0
    performEvent_ $ writeExternalRef r <$> updated da
    pure r

{- | Creates external ref as a result of "fmapping" a function to the original ref.
 ExternalRef t is not a true Functior, since it requres monadic action to "fmap"
 Editing of the new ref has no effect on the original dynamic.
-}
fmapExternalRef
    :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
    => (a -> b)
    -> ExternalRef t a
    -> m (ExternalRef t b)
fmapExternalRef f ea = do
    v0 <- readExternalRef ea
    r <- newExternalRef $ f v0
    performEvent_ $ writeExternalRef r . f <$> externalEvent ea
    pure r
