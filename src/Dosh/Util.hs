module Dosh.Util where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Graphics.Vty (Key (..), Modifier (..))
import Reflex
import Reflex.Vty
import System.IO (Handle, hReady)

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

tread :: Read a => Text -> a
tread = read . Text.unpack

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

{- | Given a map of values and a map of value transformations, apply
 transformations on the intersection of these two maps.
-}
transformMap :: Map Int (c -> c) -> Map Int c -> Map Int c
transformMap =
    Map.mergeWithKey
        (const (Just .))
        (const mempty)
        id

getAvailableLines :: Handle -> IO [Text]
getAvailableLines handle = do
    hasOutput <- hReady handle
    if hasOutput
        then (:) <$> Text.hGetLine handle <*> getAvailableLines handle
        else pure []

getLines :: Handle -> IO [Text]
getLines handle =
    (:)
        <$> Text.hGetLine handle
        <*> getAvailableLines handle
