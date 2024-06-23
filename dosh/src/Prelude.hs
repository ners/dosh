{-# OPTIONS_GHC -Wno-orphans #-}

module Prelude
    ( module Prelude
    , module Dosh.Prelude
    , module FRP.Rhine
    , module Data.Text.Rope
    , module Data.ExtendedReal
    , module Data.Interval
    , module Prettyprinter
    )
where

import Data.ExtendedReal (Extended)
import Data.ExtendedReal qualified as Extended
import Data.Interval (Interval)
import Data.Interval qualified as Interval
import Data.Text.Rope (Rope)
import Data.Text.Rope qualified as Rope
import Dosh.Prelude
import FRP.Rhine hiding (integral, mapMaybe, newChan, try)
import Prettyprinter (Doc, Pretty (pretty), annotate, pretty)
import System.IO.Unsafe (unsafePerformIO)

infixl 4 <$$>

(<$$>) :: (Functor f1) => (Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

infixl 1 <&&>

(<&&>) :: (Functor f1) => (Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<&&>) = flip (<$$>)

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

fromText :: (IsString s) => Text -> s
fromText = fromString . fromText

integral :: (Integral a, Integral b) => Iso' a b
integral = iso fromIntegral fromIntegral

ropeText :: Iso' Rope Text
ropeText = iso Rope.toText Rope.fromText

-- | Clamp the Extended value between the given finite bounds.
clampExtended :: (Ord r) => (r, r) -> Extended r -> r
clampExtended (a, b) r =
    case r of
        Extended.NegInf -> a
        Extended.Finite f -> clamp (a, b) f
        Extended.PosInf -> b

mapInterval :: (Ord b) => (a -> b) -> Interval a -> Interval b
mapInterval f i =
    Interval.interval
        (first (fmap f) $ Interval.lowerBound' i)
        (first (fmap f) $ Interval.upperBound' i)

{-# NOINLINE dirtyLogger #-}
dirtyLogger :: TQueue String
dirtyLogger = unsafePerformIO do
    q <- newTQueueIO
    forkIO . forever $ atomically (readTQueue q) >>= appendFile "log.txt"
    pure q

dirtyLog :: String -> a -> a
dirtyLog = seq . unsafePerformIO . atomically . writeTQueue dirtyLogger . (<> "\n")

instance Pretty Rope where
    pretty = pretty . Rope.toText

ropeLines :: Rope -> [Rope]
ropeLines "" = [""]
ropeLines (Rope.splitAtLine 1 -> (a, b)) = a : ropeLines b
