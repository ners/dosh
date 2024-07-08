{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Position where

import Control.Lens
import Data.Generics.Labels ()
import Data.Integral
import Data.Text.Rope qualified as Rope
import GHC.Generics
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import System.Terminal qualified as Terminal
import "base" Prelude

class Position p where
    row :: Lens' p Int
    col :: Lens' p Int
    fromPosition :: (Position p') => p' -> p

position :: (Position p1, Position p2) => Iso' p1 p2
position = Control.Lens.iso fromPosition fromPosition

instance Position (Int, Int) where
    row = _1
    col = _2
    fromPosition p = (p ^. row, p ^. col)

instance Position LSP.Position where
    row = LSP.line . integral
    col = LSP.character . integral
    fromPosition p = LSP.Position{_line = p ^. row . integral, _character = p ^. col . integral}

deriving stock instance Generic Rope.Position

instance Position Rope.Position where
    row = #posLine . integral
    col = #posColumn . integral
    fromPosition p = Rope.Position{posLine = p ^. row . integral, posColumn = p ^. col . integral}

deriving stock instance Generic Terminal.Position

instance Position Terminal.Position where
    row = #row
    col = #col
    fromPosition p = Terminal.Position{row = p ^. row, col = p ^. col}
