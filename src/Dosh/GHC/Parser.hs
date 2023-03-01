{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.Parser
    ( module GHC.Types.SrcLoc
    , Code
    , Chunk (..)
    , CodeChunk
    , splitChunks
    , splitExpressions
    , locatedText
    , locatedLines
    , locatedUnlines
    )
where

import Data.Text qualified as Text
import Development.IDE (ParseResult (..), runParser)
import Development.IDE.GHC.Compat.Core (DynFlags, GhcMonad, GhcPs, HsModule (..), LHsDecl, getSessionDynFlags, parseExpression, parseModule)
import Dosh.Prelude
import Dosh.Util
import GHC.Data.FastString (FastString)
import GHC.Types.SrcLoc

type Code = RealLocated Text

#if !MIN_VERSION_ghc(9,4,0)
deriving stock instance Show Code
#endif

data Chunk
    = ModuleChunk Code
    | DeclarationChunk [LHsDecl GhcPs]
    | ExpressionChunk [Code]

type CodeChunk = RealLocated Chunk

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
splitChunks :: GhcMonad m => Code -> m [CodeChunk]
splitChunks code = do
    let chunks = reverse $ foldl' appendLine [] $ locatedLines code
    mapM decideChunk chunks
  where
    appendLine :: [Code] -> Code -> [Code]
    appendLine [] line = [line]
    appendLine (c : cs) line
        | maybeStartsWith True isSpace (unLoc line) || not (isNewline c) =
            locatedUnlines [c, line] : cs
        | otherwise = line : c : cs
    isNewline :: Code -> Bool
    isNewline = maybeEndsWith True (== '\n') . unLoc

decideChunk :: forall m. GhcMonad m => Code -> m CodeChunk
decideChunk c@(L loc _) = do
    flags <- getSessionDynFlags
    let def = ExpressionChunk [c]
        parsed = foldr1 (<|>) $ [parseModuleChunk, parseExprChunk, parseDeclChunk] <&> (c &) . (flags &)
    pure $ L loc $ fromMaybe def parsed

parseModuleChunk :: DynFlags -> Code -> Maybe Chunk
parseModuleChunk flags c@(Text.unpack . unLoc -> code) = case runParser flags code parseModule of
    POk _ (unLoc -> (hsmodName -> Just _)) -> Just $ ModuleChunk c
    _ -> Nothing

parseExprChunk :: DynFlags -> Code -> Maybe Chunk
parseExprChunk flags (splitExpressions -> exprs)
    | all isExpr exprs = Just $ ExpressionChunk exprs
    | otherwise = Nothing
  where
    isExpr (Text.unpack . unLoc -> "") = True
    isExpr (Text.unpack . unLoc -> code) = case runParser flags code parseExpression of
        POk{} -> True
        _ -> False

parseDeclChunk :: DynFlags -> Code -> Maybe Chunk
parseDeclChunk flags (Text.unpack . unLoc -> code) = case runParser flags code parseModule of
    POk _ (hsmodDecls . unLoc -> decls) -> Just $ DeclarationChunk decls
    _ -> Nothing

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
