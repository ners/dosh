module Dosh.Util where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty hiding (Event)
import Reflex
import Reflex.ExternalRef
import Reflex.Vty

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip (<$$>)

tshow :: Show a => a -> Text
tshow = Text.pack . show

ctrlcPressed :: (Monad m, Reflex t, HasInput t m) => m (Event t KeyCombo)
ctrlcPressed = keyCombo (KChar 'c', [MCtrl])

ctrldPressed :: (Monad m, Reflex t, HasInput t m) => m (Event t KeyCombo)
ctrldPressed = keyCombo (KChar 'd', [MCtrl])

enterPressed :: (Monad m, Reflex t, HasInput t m) => m (Event t KeyCombo)
enterPressed = key KEnter

shiftEnterPressed :: (Monad m, Reflex t, HasInput t m) => m (Event t KeyCombo)
shiftEnterPressed = keyCombo (KEnter, [MShift])

{- | Create a new 'Event' that occurs if at least one of the 'Event's in the
 map occurs. If multiple occur at the same time the value is the value of the
 minimal event.
-}
minmost :: Reflex t => Map a (Event t b) -> Event t (a, b)
minmost = maybe never (\(a, eb) -> (a,) <$> eb) . Map.lookupMin

echoServer
    :: forall t m
     . (Reflex t, MonadIO m, PerformEvent t m, TriggerEvent t m)
    => m (ExternalRef t Text, ExternalRef t Text)
echoServer = do
    i <- newExternalRef
    o <- newExternalRef
    void $ liftIO $ forkIO $ forever $ do
        incomingText <- readExternalRef i
        forM_ (Text.inits incomingText) $ \prefix -> do
            writeExternalRef o prefix
            liftIO $ threadDelay 100_000
    pure (i, o)
