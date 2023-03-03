{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.LexerSpec where

import Data.Text qualified as Text
import Dosh.GHC.Lexer
import Dosh.Prelude hiding (elements)
import Test.Hspec
import Test.Hspec.Expectations.Extra
import Test.QuickCheck

atLine :: Text -> Int -> Chunk
atLine = flip $ chunkFromText "<test>"

shouldStartOnLine :: RealSrcSpan -> Int -> Expectation
loc `shouldStartOnLine` l = startLine loc `shouldBe` l

startLine :: RealSrcSpan -> Int
startLine = srcLocLine . realSrcSpanStart

endLine :: RealSrcSpan -> Int
endLine = srcLocLine . realSrcSpanEnd

locate :: Text -> Expectation
locate (flip atLine 1 -> L loc code) = do
    srcSpanFile loc `shouldBe` "<test>"
    srcLocLine start `shouldBe` 1
    srcLocCol start `shouldBe` 1
    srcLocLine end `shouldBe` length codeLines
    srcLocCol end `shouldBe` max 1 (Text.length (last codeLines))
  where
    start = realSrcSpanStart loc
    end = realSrcSpanEnd loc
    codeLines = Text.splitOn "\n" code

chunksAreNotAdjacent :: Text -> Expectation
chunksAreNotAdjacent (flip atLine 1 -> c) = do
    let chunks = splitChunks c
    forM_ (zip chunks $ tail chunks) $ \(L loc1 _, L loc2 _) -> do
        loc2 `shouldStartOnLine` (endLine loc1 + 1)

expressionsAreAdjacent :: Text -> Expectation
expressionsAreAdjacent (flip atLine 1 -> c) = do
    let exprs = splitExpressions c
    forM_ (zip exprs $ tail exprs) $ \(L loc1 expr1, L loc2 _) -> do
        loc2 `shouldStartOnLine` (endLine loc1 + 1)
        expr1 `shouldNotEndWithText` "\n"

instance Arbitrary Text where
    arbitrary = Text.pack <$> listOf (elements "xxx \n")

spec :: Spec
spec = do
    it "correctly locates code" $ property locate
    it "does not produce adjacent chunks" $ property chunksAreNotAdjacent
    it "produces adjacent expressions" $ property expressionsAreAdjacent
