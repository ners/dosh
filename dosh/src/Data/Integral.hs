module Data.Integral where

import Control.Lens
import "base" Prelude

integral :: (Integral a, Integral b) => Iso' a b
integral = iso fromIntegral fromIntegral
