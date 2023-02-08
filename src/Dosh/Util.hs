module Dosh.Util where

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty hiding (Event)
import Reflex
import Reflex.Vty

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip (<$$>)

(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<$$) = fmap . (<$)

($$>) :: (Functor f, Functor g) => f (g a) -> b -> f (g b)
($$>) = flip (<$$)

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

data IoServer t = IoServer
    { query :: (Int, Text) -> IO ()
    , response :: Event t (Int, Text)
    }

echoServer
    :: forall t m
     . ( Reflex t
       , MonadIO m
       , PerformEvent t m
       , TriggerEvent t m
       , MonadIO (Performable m)
       , PostBuild t m
       , MonadFix m
       )
    => m (IoServer t)
echoServer = do
    (queryEvent, queryTrigger) <- newTriggerEvent
    (responseEvent, responseTrigger) <- newTriggerEvent
    performEvent $ queryEvent <&> liftIO . forkIO . handler responseTrigger
    pure
        IoServer
            { query = queryTrigger
            , response = responseEvent
            }
    where
        handler :: ((Int, Text) -> IO ()) -> (Int, Text) -> IO ()
        handler responseTrigger (i, query) =
            forM_ (Text.singleton <$> Text.unpack query) $ \t -> do
                threadDelay 500_000
                responseTrigger (i, t)
