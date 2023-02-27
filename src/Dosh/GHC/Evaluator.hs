module Dosh.GHC.Evaluator where

import Control.Monad.Extra (unlessM)
import Data.Text qualified as Text
import Development.IDE (ParseResult (..), runParser)
import Development.IDE.GHC.Compat.Core (GhcMonad, HsModule (..), getSessionDynFlags, parseExpression, parseModule)
import Dosh.GHC.Parser
import Dosh.Prelude hiding (mod)
import GHC (execOptions, execStmt, runParsedDecls)

evaluate :: GhcMonad m => Text -> m ()
evaluate (locatedText "<interactive>" 1 -> code) = do
    flags <- getSessionDynFlags
    case runParser flags (Text.unpack $ unLoc code) parseModule of
        POk _ (unLoc -> mod@(hsmodName -> Just _)) -> evaluateModule mod
        _ -> mapM_ evaluateChunk $ splitChunks code

evaluateModule :: GhcMonad m => HsModule -> m ()
evaluateModule _ = void $ execStmt "putStrLn \"Modules are not yet supported\"" execOptions

evaluateChunk :: GhcMonad m => Code -> m ()
evaluateChunk (Text.null . unLoc -> True) = pure ()
evaluateChunk chunk = tryParsers [tryEvaluateExpressions, tryEvaluateDecls]
  where
    tryParsers (p : ps) = unlessM (p chunk) $ tryParsers ps
    tryParsers [] = void $ execStmt "putStrLn \"Could not parse chunk!\"" execOptions

tryEvaluateExpressions :: GhcMonad m => Code -> m Bool
tryEvaluateExpressions (fmap (Text.unpack . unLoc) . filter (not . Text.null . unLoc) . splitExpressions -> exprs) = do
    flags <- getSessionDynFlags
    let isExpr s = case runParser flags s parseExpression of
            POk{} -> True
            _ -> False
    if all isExpr exprs
        then do
            forM_ exprs $ \e -> execStmt e execOptions
            pure True
        else pure False

tryEvaluateDecls :: GhcMonad m => Code -> m Bool
tryEvaluateDecls chunk = do
    flags <- getSessionDynFlags
    case runParser flags (Text.unpack $ unLoc chunk) parseModule of
        POk _ (unLoc -> HsModule{..}) -> do
            runParsedDecls hsmodDecls
            pure True
        _ -> pure False
