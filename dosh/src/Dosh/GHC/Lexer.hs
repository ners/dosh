{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.Lexer
    ( module GHC.Types.SrcLoc
    , Chunk
    , chunkFromText
    , splitChunks
    , splitExpressions
    , chunkLines
    , chunkUnlines
    , firstLine
    , lastLine
    )
where

import Data.Text qualified as Text
import Dosh.Prelude
import Dosh.Util
import GHC.Data.FastString
import GHC.Types.SrcLoc

type Chunk = RealLocated Text

#if !MIN_VERSION_ghc(9,4,0)
deriving stock instance Show Chunk
#endif

chunkFromText :: FastString -> Int -> Text -> Chunk
chunkFromText file firstLine t = L (mkRealSrcSpan a b) t
  where
    a = mkRealSrcLoc file firstLine 1
    b = mkRealSrcLoc file (firstLine + Text.count "\n" t) (max 1 $ Text.length $ last ts)
    ts = Text.splitOn "\n" t

{- | Split a code object into chunks. A chunk is a sequence of lines that should be evaluated in the same way;
  either as a module or as expressions.

  Chunks are separated by an empty line followed by a graphical character on the zero column.

  An example of code with four chunks that parse as expected:

  > {\-# LANGUAGE OverloadedStrings #-\}
  >
  > import Dosh.Prelude
  > import Data.Text qualified as Text
  >
  > startsWith :: (Char -> Bool) -> Text -> Bool
  > startsWith f = maybe False f . Text.uncons
  >
  > startsWith isSpace "example"

  If we remove the last empty line between the declaration and function call,
  the chunks will be merged and parsed as one:

   > startsWith :: (Char -> Bool) -> Text -> Bool
   > startsWith f = maybe False f . Text.uncons
   > startsWith isSpace "example"

  This will cause a parsing error, because expressions are not allowed in a module chunk.
-}
splitChunks :: Chunk -> [Chunk]
splitChunks chunk = reverse $ foldl' appendLine [] $ chunkLines chunk
  where
    appendLine :: [Chunk] -> Chunk -> [Chunk]
    appendLine [] line = [line]
    appendLine (c : cs) line
        | maybeStartsWith True isSpace (unLoc line) || not (isNewline c) =
            chunkUnlines [c, line] : cs
        | otherwise = line : c : cs
    isNewline :: Chunk -> Bool
    isNewline = maybeEndsWith True (== '\n') . unLoc

{- | Split a code object into expressions.

  Expressions are lines that start with a graphical character on the zero column.
-}
splitExpressions :: Chunk -> [Chunk]
splitExpressions chunk = reverse $ foldl' appendLine [] $ chunkLines chunk
  where
    appendLine :: [Chunk] -> Chunk -> [Chunk]
    appendLine [] line = [line]
    appendLine (e : es) line
        | maybeStartsWith False isSpace (unLoc line) =
            chunkUnlines [e, line] : es
        | otherwise = line : e : es

chunkLines :: Chunk -> [Chunk]
chunkLines c@(L loc t)
    | null ts = [L loc ""]
    | otherwise = uncurry line <$> zip [0 ..] ts
  where
    file = srcSpanFile loc
    line i = chunkFromText file (firstLine c + i)
    ts = Text.splitOn "\n" t

chunkUnlines :: [Chunk] -> Chunk
chunkUnlines ts@((realSrcSpanStart . getLoc -> start) : _) = chunkFromText (srcLocFile start) (srcLocLine start) $ Text.intercalate "\n" $ unLoc <$> ts
chunkUnlines [] = error "locatedUnlines: cannot locate empty list"

firstLine :: GenLocated RealSrcSpan e -> Int
firstLine = srcLocLine . realSrcSpanStart . getLoc

lastLine :: GenLocated RealSrcSpan e -> Int
lastLine = srcLocLine . realSrcSpanEnd . getLoc
