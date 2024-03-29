{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.Util where

import Control.Monad.Extra (whenMaybeM, whileJustM)
import Control.Monad.Fix (MonadFix)
import Data.ByteString (hGetSome)
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Development.IDE (Priority, WithPriority (..))
import Dosh.Prelude
import Graphics.Vty (Key (..), Modifier (..))
import Language.LSP.Types (Diagnostic, Uri (..), filePathToUri)
import Language.LSP.Types.Lens (HasLine (line), character, range, start)
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

getAvailableContents :: Handle -> IO ByteString
getAvailableContents h = whileJustM $ whenMaybeM (hReady h) (hGetSome h defaultChunkSize)

{- | Run two @IO@ actions concurrently.
 The loser of the race is 'cancel'led after a delay (in microseconds).
-}
raceWithDelay_ :: MonadUnliftIO m => Int -> m a -> m b -> m ()
raceWithDelay_ (threadDelay -> d) a b = race_ (a <* d) (b <* d)

attach2 :: Reflex t => (Behavior t a, Behavior t b) -> Event t c -> Event t (a, b, c)
attach2 (b1, b2) e = attach b1 (attach b2 e) <&> \(a, (b, c)) -> (a, b, c)

attach3 :: Reflex t => (Behavior t a, Behavior t b, Behavior t c) -> Event t d -> Event t (a, b, c, d)
attach3 (b1, b2, b3) e = attach b1 (attach2 (b2, b3) e) <&> \(a, (b, c, d)) -> (a, b, c, d)

blankLine :: forall t m. (HasLayout t m, HasInput t m, HasImageWriter t m, HasDisplayRegion t m, HasFocusReader t m, MonadHold t m, MonadFix m) => m ()
blankLine = grout (fixed $ pure 1) blank

toMaybe :: Bool -> a -> Maybe a
toMaybe False = const Nothing
toMaybe True = Just

maybeStartsWith :: Bool -> (Char -> Bool) -> Text -> Bool
maybeStartsWith d f = maybe d (f . fst) . Text.uncons

maybeEndsWith :: Bool -> (Char -> Bool) -> Text -> Bool
maybeEndsWith d _ "" = d
maybeEndsWith _ f t = f $ Text.last t

newlined :: Text -> Text
newlined t
    | maybeEndsWith False (== '\n') t = t
    | otherwise = t <> "\n"

withTimeout :: (MonadUnliftIO m, MonadFail m) => Int -> m a -> m a
withTimeout = ((maybe (fail "Timeout exceeded") pure =<<) .) . timeout

prependAndPadLines :: Text -> Text -> Text
prependAndPadLines x = (x <>) . Text.drop len . Text.intercalate "\n" . fmap (pad <>) . Text.splitOn "\n"
  where
    len = Text.length x
    pad = Text.replicate len " "

instance Show (WithPriority Text) where
    show WithPriority{..} = Text.unpack $ prependAndPadLines prio payload
      where
        len = (1 +) . maximum $ Text.length . tshow <$> [minBound @Priority .. maxBound]
        prio = Text.justifyLeft len ' ' $ tshow priority

diagnosticLine :: Diagnostic -> Int
diagnosticLine = fromIntegral . view (range . start . line)

diagnosticChar :: Diagnostic -> Int
diagnosticChar = fromIntegral . view (range . start . character)

withLineNumbers :: Text -> Text
withLineNumbers t = Text.intercalate "\n" $ zipWith (\i t -> Text.justifyRight w ' ' (tshow i) <> " " <> t) [0 :: Int ..] ls
  where
    ls = Text.splitOn "\n" t
    w = Text.length $ tshow $ length ls

uuidToUri :: FilePath -> FilePath -> UUID -> Uri
uuidToUri prefix postfix = filePathToUri . Text.unpack . (Text.pack prefix <>) . (<> Text.pack postfix) . UUID.toText

newRandomUri :: MonadIO m => FilePath -> FilePath -> m Uri
newRandomUri prefix postfix = uuidToUri prefix postfix <$> liftIO UUID.nextRandom
