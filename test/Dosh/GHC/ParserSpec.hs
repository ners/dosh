{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.ParserSpec where

import Dosh.Prelude hiding (elements)
import Test.Hspec
import Test.Hspec.Expectations.Extra
import Test.QuickCheck
import Dosh.GHC.Parser
import qualified Data.Text as Text

instance Show Code where
    show (L (show -> loc) code) = loc <> ": " <> show code

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

splitAndMerge :: Code -> Expectation
splitAndMerge c = locatedUnlines (splitCode c) `shouldBe` c

notAdjacent :: Code -> Expectation
notAdjacent c = do
    let chunks = splitCode c
    forM_ (zip chunks $ tail chunks) $ \(L loc1 _, L loc2 _) -> do
        loc2 `shouldStartAfterLine` endLine loc1

    where shouldStartAfterLine :: RealSrcSpan -> Int -> Expectation
          loc `shouldStartAfterLine` l = startLine loc `shouldSatisfy` (> l)

notIndented :: Code -> Expectation
notIndented c = do
    let chunks = splitCode $ locatedUnlines ["x", c]
    forM_ chunks $ \(L _ chunk) -> chunk `shouldNotStartWithText` " "

spec :: Spec
spec = do
    it "locates code correctly" $ property locate
    it "splits and merges code correctly" $ property splitAndMerge
    it "splits empty text correctly" $ property $ do
        splitCode "" `shouldBe` [""]
    it "splits single line correctly" $ property $ do
        splitCode "\n" `shouldBe` ["\n" `atLine` 1]
        splitCode "foo" `shouldBe` ["foo" `atLine` 1]
        splitCode "foo\n" `shouldBe` ["foo\n" `atLine` 1]
    it "splits one chunk correctly" $ property $ do
        splitCode "foo\nbar" `shouldBe` ["foo\nbar" `atLine` 1]
        splitCode "foo\nbar\n" `shouldBe` ["foo\nbar\n" `atLine` 1]
    it "splits two chunks correctly" $ property $ do
        splitCode "\nfoo\nbar" `shouldBe` ["" `atLine` 1, "foo\nbar" `atLine` 2]
        splitCode "foo\n\nbar" `shouldBe` ["foo\n" `atLine` 1, "bar" `atLine` 3]
    it "does not produce adjacent chunks" $ property notAdjacent
    it "does not produce indented chunks" $ property notIndented
