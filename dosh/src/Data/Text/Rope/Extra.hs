module Data.Text.Rope.Extra where

import Data.Text qualified as Text
import Data.Text.Rope qualified as Rope
import Prelude hiding (uncons)

uncons :: Rope -> Maybe (Char, Rope)
uncons = fmap (second Rope.fromText) . Text.uncons . Rope.toText

hasTrailingNewline :: Rope -> Bool
hasTrailingNewline (uncons -> Just ('\n', _)) = True
hasTrailingNewline _ = False
