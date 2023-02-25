module Dosh.GHC.Evaluator where

import Data.Text qualified as Text
import Development.IDE (ParseResult (..), runParser)
import Development.IDE.GHC.Compat.Core (GhcMonad, HsModule (..), getSessionDynFlags, parseModule)
import Dosh.GHC.Parser
import Dosh.Prelude hiding (mod)
import GHC (execOptions, execStmt)

evaluate :: GhcMonad m => Text -> m ()
evaluate (locatedText "<interactive>" 1 -> code@(Text.unpack . unLoc -> codeStr)) = do
    flags <- getSessionDynFlags
    case runParser flags codeStr parseModule of
        POk _ (unLoc -> mod@(hsmodName -> Just _)) -> evaluateModule mod
        _ -> undefined code

evaluateModule :: GhcMonad m => HsModule -> m ()
evaluateModule _ = void $ execStmt "hPutStrLn stderr \"Modules are not yet supported\"" execOptions
