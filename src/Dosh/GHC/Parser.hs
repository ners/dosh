{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.Parser
    ( module GHC.Types.SrcLoc
    , Code
    , splitChunks
    , splitExpressions
    , locatedText
    , locatedLines
    , locatedUnlines
    )
where

import Data.Text qualified as Text
import Dosh.Prelude
import Dosh.Util
import GHC.Data.FastString (FastString)
import GHC.Types.SrcLoc

type Code = RealLocated Text

#if !MIN_VERSION_ghc(9,4,0)
deriving stock instance Show Code
#endif

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
splitChunks :: Code -> [Code]
splitChunks code = reverse $ foldl' appendLine [] $ locatedLines code
  where
    appendLine :: [Code] -> Code -> [Code]
    appendLine [] line = [line]
    appendLine (c : cs) line
        | maybeStartsWith True isSpace (unLoc line) || not (isNewline c) =
            locatedUnlines [c, line] : cs
        | otherwise = line : c : cs
    isNewline :: Code -> Bool
    isNewline = maybeEndsWith True (== '\n') . unLoc

{- | Split a code object into expressions.

  Expressions are lines that start with a graphical character on the zero column.
-}
splitExpressions :: Code -> [Code]
splitExpressions code = reverse $ foldl' appendLine [] $ locatedLines code
  where
    appendLine :: [Code] -> Code -> [Code]
    appendLine [] line = [line]
    appendLine (e : es) line
        | maybeStartsWith False isSpace (unLoc line) =
            locatedUnlines [e, line] : es
        | otherwise = line : e : es

locatedText :: FastString -> Int -> Text -> RealLocated Text
locatedText file firstLine t = L (mkRealSrcSpan from to) t
  where
    from = mkRealSrcLoc file firstLine 1
    to = mkRealSrcLoc file (firstLine + Text.count "\n" t) (max 1 $ Text.length $ last ts)
    ts = Text.splitOn "\n" t

locatedLines :: RealLocated Text -> [RealLocated Text]
locatedLines (L loc t)
    | null ts = [L loc ""]
    | otherwise = uncurry line <$> zip [0 ..] ts
  where
    file = srcSpanFile loc
    firstLine = srcLocLine $ realSrcSpanStart loc
    line i = locatedText file (firstLine + i)
    ts = Text.splitOn "\n" t

locatedUnlines :: [RealLocated Text] -> RealLocated Text
locatedUnlines ts@((realSrcSpanStart . getLoc -> start) : _) = locatedText (srcLocFile start) (srcLocLine start) $ Text.intercalate "\n" $ unLoc <$> ts
locatedUnlines [] = error "locatedUnlines: cannot locate empty list"
