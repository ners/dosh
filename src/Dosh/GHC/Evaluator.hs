module Dosh.GHC.Evaluator where

import Data.Text qualified as Text
import Development.IDE.GHC.Compat.Core (GhcMonad)
import Dosh.GHC.Parser
import Dosh.Prelude hiding (mod)
import GHC (execOptions, execStmt, runParsedDecls)

evaluate :: GhcMonad m => Text -> m ()
evaluate (locatedText "<interactive>" 1 -> code) = do
    splitChunks code >>= mapM_ (evaluateChunk . unLoc)

evaluateChunk :: GhcMonad m => Chunk -> m ()
evaluateChunk (ExpressionChunk exprs) =
    forM_ exprs $ \e -> execStmt (Text.unpack $ unLoc e) execOptions
evaluateChunk (DeclarationChunk decls) =
    void $ runParsedDecls decls
evaluateChunk (ModuleChunk _) =
    void $ execStmt "putStrLn \"Modules are not yet supported\"" execOptions
