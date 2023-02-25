{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Sequence.ZipperSpec where

import Data.List (foldl')
import Data.Ord (clamp)
import Data.Sequence.Zipper
import GHC.Exts (IsList (..))
import Test.Hspec hiding (before)
import Test.QuickCheck
import Prelude

instance Arbitrary t => Arbitrary (SeqZipper t) where
    arbitrary :: Gen (SeqZipper t)
    arbitrary = do
        before <- arbitrary
        after <- arbitrary
        pure SeqZipper{..}

shouldBeEquivalentTo :: (Eq t, Show t) => SeqZipper t -> SeqZipper t -> Expectation
shouldBeEquivalentTo a b = toList a `shouldBe` toList b

isList :: forall t. (Eq t, Show t) => SeqZipper t -> Expectation
isList z = fromList (toList z) `shouldBeEquivalentTo` z

isMonoid :: (Eq t, Show t) => SeqZipper t -> SeqZipper t -> SeqZipper t -> Expectation
isMonoid a b c = (a <> b) <> c `shouldBe` a <> (b <> c)

listMonoidHomomorphism :: (Eq t, Show t) => SeqZipper t -> SeqZipper t -> Expectation
listMonoidHomomorphism a b = toList (a <> b) `shouldBe` toList a <> toList b

data Move
    = Forward
    | Back
    | Home
    | End
    deriving stock (Show, Eq, Bounded, Enum)

instance Arbitrary Move where arbitrary = elements [minBound .. maxBound]

type MoveSequence = [Move]

performMove :: Move -> SeqZipper t -> SeqZipper t
performMove Forward = forward
performMove Back = back
performMove Home = home
performMove End = end

performMoves :: SeqZipper t -> MoveSequence -> SeqZipper t
performMoves = foldl' (flip performMove)

unchangedByMovement :: (Eq t, Show t) => SeqZipper t -> MoveSequence -> Expectation
unchangedByMovement zipper moves = performMoves zipper moves `shouldBeEquivalentTo` zipper

itMoves :: Move -> SeqZipper t -> Expectation
itMoves m zipper = newPosition `shouldBe` expectedNewPosition
  where
    position = length (before zipper)
    newZipper = performMove m zipper
    newPosition = length (before newZipper)
    expectedNewPosition = clamp (0, length zipper) $ case m of
        Forward -> position + 1
        Back -> position - 1
        Home -> 0
        End -> maxBound

spec :: Spec
spec = do
    it "is a list" $ property $ isList @Int
    it "is a monoid" $ property $ isMonoid @Int
    it "list is a monoid homomorphism" $ property $ listMonoidHomomorphism @Int
    it "is unchanged by movement" $ property $ unchangedByMovement @Int
    it "correctly handles movement" $ property $ itMoves @Int
