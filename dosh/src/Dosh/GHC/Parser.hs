{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.Parser where

import Data.Text qualified as Text
import Development.IDE (runParser)
import Development.IDE.GHC.Compat.Core (DynFlags, GhcMonad, GhcPs, HsModule (..), LHsDecl, getSessionDynFlags, parseExpression, parseModule)
import Dosh.GHC.Lexer
import Dosh.Prelude
import GHC.Parser.Lexer

data ParsedChunk
    = ModuleChunk HsModule
    | ExpressionChunk [Chunk]
    | DeclarationChunk [LHsDecl GhcPs]

parseChunk :: forall m. GhcMonad m => Chunk -> m ParsedChunk
parseChunk c = do
    flags <- getSessionDynFlags
    let def = ExpressionChunk [c]
        parsed = foldr1 (<|>) $ [parseModuleChunk, parseExprChunk, parseDeclChunk] <&> (c &) . (flags &)
    pure $ fromMaybe def parsed

parseModuleChunk :: DynFlags -> Chunk -> Maybe ParsedChunk
parseModuleChunk flags (Text.unpack . unLoc -> code) = case runParser flags code parseModule of
    POk _ (unLoc -> mod@(hsmodName -> Just _)) -> Just $ ModuleChunk mod
    _ -> Nothing

parseExprChunk :: DynFlags -> Chunk -> Maybe ParsedChunk
parseExprChunk flags (splitExpressions -> exprs)
    | all isExpr exprs = Just $ ExpressionChunk exprs
    | otherwise = Nothing
  where
    isExpr (unLoc -> "") = True
    isExpr (Text.unpack . unLoc -> code) = case runParser flags code parseExpression of
        POk{} -> True
        _ -> False

parseDeclChunk :: DynFlags -> Chunk -> Maybe ParsedChunk
parseDeclChunk flags (Text.unpack . unLoc -> code) = case runParser flags code parseModule of
    POk _ (hsmodDecls . unLoc -> decls) -> Just $ DeclarationChunk decls
    _ -> Nothing
