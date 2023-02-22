module Dosh.Util where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (race_)
import Control.Lens
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
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

tread :: Read a => Text -> a
tread = read . Text.unpack

tshow :: Show a => a -> Text
tshow = Text.pack . show

bshow :: Show a => a -> ByteString
bshow = Text.encodeUtf8 . tshow

fromText :: IsString s => Text -> s
fromText = fromString . Text.unpack

ctrlcPressed :: (Monad m, Reflex t, HasInput t m) => m (Event t KeyCombo)
ctrlcPressed = keyCombo (KChar 'c', [MCtrl])

ctrldPressed :: (Monad m, Reflex t, HasInput t m) => m (Event t KeyCombo)
ctrldPressed = keyCombo (KChar 'd', [MCtrl])

enterPressed :: (Monad m, Reflex t, HasInput t m) => m (Event t KeyCombo)
enterPressed = key KEnter

shiftEnterPressed :: (Monad m, Reflex t, HasInput t m) => m (Event t KeyCombo)
shiftEnterPressed = keyCombo (KEnter, [MShift])

altEnterPressed :: (Monad m, Reflex t, HasInput t m) => m (Event t KeyCombo)
altEnterPressed = keyCombo (KEnter, [MMeta])

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

{- | Run two @IO@ actions concurrently.
 The loser of the race is 'cancel'led after a delay (in microseconds).
-}
raceWithDelay_ :: MonadBaseControl IO m => Int -> m a -> m b -> m ()
raceWithDelay_ d a b = race_ (a <* delay') (b <* delay')
  where
    delay' = liftBase $ threadDelay d

attach2 :: Reflex t => (Behavior t a, Behavior t b) -> Event t c -> Event t (a, b, c)
attach2 (b1, b2) e = attach b1 (attach b2 e) <&> \(a, (b, c)) -> (a, b, c)

attach3 :: Reflex t => (Behavior t a, Behavior t b, Behavior t c) -> Event t d -> Event t (a, b, c, d)
attach3 (b1, b2, b3) e = attach b1 (attach2 (b2, b3) e) <&> \(a, (b, c, d)) -> (a, b, c, d)

blankLine :: forall t m. (Reflex t, Monad m, HasLayout t m, HasInput t m, HasImageWriter t m, HasDisplayRegion t m, HasFocusReader t m) => m ()
blankLine = grout (fixed $ pure 1) blank
