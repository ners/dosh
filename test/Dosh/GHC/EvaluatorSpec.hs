{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.EvaluatorSpec where

import Data.Text.Encoding qualified as Text
import Development.IDE.GHC.Compat (Ghc)
import Dosh.GHC.Evaluator qualified as GHC
import Dosh.GHC.ParserSpec
import Dosh.GHC.Server qualified as GHC
import Dosh.Prelude hiding (elements)
import Dosh.Util
import Test.Hspec
import Test.QuickCheck

runGhcSession :: Ghc () -> IO (ByteString, ByteString, [SomeException])
runGhcSession action = do
    GHC.testServer $ do
        action
        GHC.evaluate "mapM_ hFlush [stdout, stderr]"

quietly :: Property -> IO ()
quietly =
    quickCheckWithResult (stdArgs{chatty = False}) >=> \case
        Success{} -> pure ()
        Failure{output} -> expectationFailure output
        GaveUp{output} -> expectationFailure output
        NoExpectedFailure{output} -> expectationFailure output

evalValidChunks :: SourceChunk -> Expectation
evalValidChunks chunk =
    withTimeout (1_000_000 + 100_000 * length chunk.srcs) $
        runGhcSession (GHC.evaluate $ tshow chunk) `shouldReturn` (expectedOutput, "", [])
  where
    expectedOutput = mconcat [Text.encodeUtf8 o | ExpressionSource _ o <- chunk.srcs]

spec :: Spec
spec = it "correctly evaluates chunks" $ quietly $ property evalValidChunks
