module Dosh.GHC.Evaluator where

import Data.Text qualified as Text
import Development.IDE (ParseResult (..), runParser)
import Development.IDE.GHC.Compat.Core (GhcMonad, HsModule (..), getSessionDynFlags, parseModule)
import Dosh.GHC.Parser
import Dosh.Prelude hiding (mod)
import GHC (execOptions, execStmt)

evaluate :: GhcMonad m => Text -> m ()
evaluate (locatedText "<interactive>" 1 -> code) = do
    flags <- getSessionDynFlags
    case runParser flags (Text.unpack $ unLoc code) parseModule of
        POk _ (unLoc -> mod@(hsmodName -> Just _)) -> evaluateModule mod
        _ -> mapM_ evaluateChunk $ splitChunks code

evaluateModule :: GhcMonad m => HsModule -> m ()
evaluateModule _ = void $ execStmt "putStrLn \"Modules are not yet supported\"" execOptions

evaluateChunk :: GhcMonad m => Code -> m ()
evaluateChunk code = void $ execStmt (Text.unpack $ unLoc code) execOptions
