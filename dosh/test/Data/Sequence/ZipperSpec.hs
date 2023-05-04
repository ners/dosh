{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Sequence.ZipperSpec where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Data.Ord (clamp)
import Data.Sequence qualified as Seq
import Data.Sequence.Zipper hiding (length)
import Data.Sequence.Zipper qualified as SZ
import GHC.Exts (IsList (..))
import Test.Hspec hiding (after, before)
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
    | ForwardWhile Predicate
    | BackWhile Predicate
    deriving stock (Show, Eq)

data Predicate
    = NotEqualZero
    | Even
    deriving stock (Show, Eq, Bounded, Enum)

predicate :: Integral t => Predicate -> (t -> Bool)
predicate NotEqualZero = (/= 0)
predicate Even = even

instance Arbitrary Move where
    arbitrary = elements $ Forward : Back : Home : End : ([ForwardWhile, BackWhile] <*> [minBound .. maxBound])

type MoveSequence = [Move]

performMove :: (Integral t) => Move -> SeqZipper t -> SeqZipper t
performMove Forward = forward
performMove Back = back
performMove Home = home
performMove End = end
performMove (ForwardWhile (predicate -> p)) = forwardWhile p
performMove (BackWhile (predicate -> p)) = backWhile p

performMoves :: (Integral t) => SeqZipper t -> MoveSequence -> SeqZipper t
performMoves = foldl' (flip performMove)

unchangedByMovement :: (Integral t, Show t) => SeqZipper t -> MoveSequence -> Expectation
unchangedByMovement zipper moves = performMoves zipper moves `shouldBeEquivalentTo` zipper

itMoves :: (Integral t, Show t) => Move -> SeqZipper t -> Expectation
itMoves m zipper = do
    newPosition `shouldBe` expectedNewPosition
    case m of
        Forward -> current newZipper `shouldBe` listToMaybe (Prelude.drop 1 forwardList)
        Back -> current newZipper `shouldBe` listToMaybe backwardList <|> current zipper
        Home -> current newZipper `shouldBe` listToMaybe (toList zipper)
        End -> current newZipper `shouldBe` Nothing
        ForwardWhile (predicate -> p) -> current newZipper `shouldSatisfy` maybe True (not . p)
        BackWhile (predicate -> p) -> unless (null $ before newZipper) $ back newZipper `shouldSatisfy` maybe False (not . p) . current
  where
    position = Seq.length (before zipper)
    newZipper = performMove m zipper
    newPosition = Seq.length (before newZipper)
    expectedNewPosition = clamp (0, SZ.length zipper) $ case m of
        Forward -> position + 1
        Back -> position - 1
        Home -> 0
        End -> maxBound
        ForwardWhile (predicate -> p) -> position + length (takeWhile p forwardList)
        BackWhile (predicate -> p) -> position - length (takeWhile p backwardList)
    forwardList = toList $ after zipper
    backwardList = reverse $ toList $ before zipper

spec :: Spec
spec = do
    it "is a list" $ property $ isList @Int
    it "is a monoid" $ property $ isMonoid @Int
    it "list is a monoid homomorphism" $ property $ listMonoidHomomorphism @Int
    it "is unchanged by movement" $ property $ unchangedByMovement @Int
    it "correctly handles movement" $ property $ itMoves @Int
