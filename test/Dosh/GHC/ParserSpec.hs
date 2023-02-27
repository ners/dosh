{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.ParserSpec where

import Data.Text qualified as Text
import Dosh.GHC.Parser
import Dosh.Prelude hiding (elements)
import Test.Hspec
import Test.Hspec.Expectations.Extra
import Test.QuickCheck

atLine :: Text -> Int -> Code
atLine = flip $ locatedText "<test>"

instance IsString Code where
    fromString = Text.pack >>> (`atLine` 1)

instance Arbitrary Code where
    arbitrary = fromString <$> listOf (elements "xxx \n")

startLine :: RealSrcSpan -> Int
startLine = srcLocLine . realSrcSpanStart

endLine :: RealSrcSpan -> Int
endLine = srcLocLine . realSrcSpanEnd

locate :: Code -> Expectation
locate (L loc code) = do
    srcSpanFile loc `shouldBe` "<test>"
    srcLocLine start `shouldBe` 1
    srcLocCol start `shouldBe` 1
    srcLocLine end `shouldBe` length codeLines
    srcLocCol end `shouldBe` max 1 (Text.length (last codeLines))
  where
    start = realSrcSpanStart loc
    end = realSrcSpanEnd loc
    codeLines = Text.splitOn "\n" code

splitAndMergeChunks :: Code -> Expectation
splitAndMergeChunks c = locatedUnlines (splitChunks c) `shouldBe` c

splitAndMergeExpressions :: Code -> Expectation
splitAndMergeExpressions c = locatedUnlines (splitExpressions c) `shouldBe` c

shouldStartOnLine :: RealSrcSpan -> Int -> Expectation
loc `shouldStartOnLine` l = startLine loc `shouldBe` l

chunksAreNotAdjacent :: Code -> Expectation
chunksAreNotAdjacent c = do
    let chunks = splitChunks c
    forM_ (zip chunks $ tail chunks) $ \(L loc1 chunk1, L loc2 _) -> do
        loc2 `shouldStartOnLine` (endLine loc1 + 1)
        unless (Text.null chunk1) $ chunk1 `shouldEndWithText` "\n"

expressionsAreAdjacent :: Code -> Expectation
expressionsAreAdjacent c = do
    let exprs = splitExpressions c
    forM_ (zip exprs $ tail exprs) $ \(L loc1 expr1, L loc2 _) -> do
        loc2 `shouldStartOnLine` (endLine loc1 + 1)
        expr1 `shouldNotEndWithText` "\n"

chunksAreNotIndented :: Code -> Expectation
chunksAreNotIndented c = do
    let chunks = splitChunks $ locatedUnlines ["x", c]
    forM_ chunks $ \(L _ chunk) -> chunk `shouldNotStartWithText` " "

expressionsAreNotIndented :: Code -> Expectation
expressionsAreNotIndented c = do
    let exprs = splitExpressions $ locatedUnlines ["x", c]
    forM_ exprs $ \(L _ expr) -> expr `shouldNotStartWithText` " "

spec :: Spec
spec = do
    it "locates code correctly" $ property locate
    it "splits and merges chunks correctly" $ property splitAndMergeChunks
    it "splits and merges expressions correctly" $ property splitAndMergeExpressions
    it "splits empty text correctly" $ property $ do
        splitChunks "" `shouldBe` [""]
    it "splits single line correctly" $ property $ do
        splitChunks "\n" `shouldBe` ["\n" `atLine` 1]
        splitChunks "foo" `shouldBe` ["foo" `atLine` 1]
        splitChunks "foo\n" `shouldBe` ["foo\n" `atLine` 1]
    it "splits one chunk correctly" $ property $ do
        splitChunks "foo\nbar" `shouldBe` ["foo\nbar" `atLine` 1]
        splitChunks "foo\nbar\n" `shouldBe` ["foo\nbar\n" `atLine` 1]
    it "splits two chunks correctly" $ property $ do
        splitChunks "\nfoo\nbar" `shouldBe` ["" `atLine` 1, "foo\nbar" `atLine` 2]
        splitChunks "foo\n\nbar" `shouldBe` ["foo\n" `atLine` 1, "bar" `atLine` 3]
    it "does not produce adjacent chunks" $ property chunksAreNotAdjacent
    it "produces adjacent expressions" $ property expressionsAreAdjacent
    it "does not produce indented chunks" $ property chunksAreNotIndented
    it "does not produce indented expressions" $ property expressionsAreNotIndented
