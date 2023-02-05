module Util where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty hiding (Event)
import Reflex
import Reflex.Network (networkView)
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

functorMapToList :: Functor f => Map a (f b) -> [f (a, b)]
functorMapToList m = uncurry (fmap . (,)) <$> Map.toList m

dyn :: (NotReady t m, Adjustable t m, PostBuild t m) => Dynamic t (m a) -> m (Event t a)
dyn = networkView
