{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Text.CodeZipperSpec where

import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (AssertionFailed (..), throw)
import Data.Char (isSpace)
import Data.Either.Extra (fromEither)
import Data.Function ((&))
import Data.List (uncons)
import Data.List.Extra (groupOn)
import Data.Ord (clamp)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.CodeZipper
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Utf8
import Prelude hiding (Left, Right)

data Printable = Graphic | Whitespace
    deriving (Eq, Show)

type PrintableToken = Token Printable

instance Arbitrary PrintableToken where
    arbitrary = oneof [arbitraryGraphic, arbitraryWhitespace]
      where
        arbitraryGraphic = do
            tokenContent <- Text.pack <$> listOf1 (elements ['a' .. 'z'])
            pure Token{tokenType = Graphic, ..}
        arbitraryWhitespace = do
            tokenContent <- Text.pack <$> listOf1 (elements [' ', '\t'])
            pure Token{tokenType = Whitespace, ..}

type PrintableLine = SourceLine Printable

arbitraryLine :: (Arbitrary (Token t), Eq t) => Gen (SourceLine t)
arbitraryLine = normaliseToks <$> listOf arbitrary

instance Pretty Printable where
    plain = fmap onSpace . Text.split (== '\n')
      where
        onSpace "" = []
        onSpace t = case Text.break isSpace t of
            ("", rt) -> onGraph rt
            (t', rt) -> Token Graphic t' : onGraph rt
        onGraph "" = []
        onGraph t = case Text.break (not . isSpace) t of
            ("", rt) -> onSpace rt
            (t', rt) -> Token Whitespace t' : onSpace rt
    pretty _ = Just . plain

instance Arbitrary Text where
    arbitrary = genValidUtf8

instance (Arbitrary (Token t), Eq t) => Arbitrary (CodeZipper t) where
    arbitrary = do
        linesBefore <- listOf arbitraryLine
        linesAfter <- listOf arbitraryLine
        tokensBefore <- arbitraryLine
        tokensAfter <- arbitraryLine
        pure CodeZipper{language = "", ..}

data Move = Up | Down | Left | Right | Home | End | Top | Bottom
    deriving (Bounded, Enum, Eq, Show)

move :: Eq t => Move -> CodeZipper t -> CodeZipper t
move Up = up
move Down = down
move Left = left
move Right = right
move Home = home
move End = end
move Top = top
move Bottom = bottom

tryMove :: Eq t => Move -> CodeZipper t -> IO (CodeZipper t)
tryMove m = within' 100 . pure . move m

within' :: Int -> IO a -> IO a
within' ms e = do
    winner <- race e $ do
        threadDelay ms
        throw $ AssertionFailed "Timeout exceeded"
    pure $ fromEither winner

instance Arbitrary Move where arbitrary = elements [minBound .. maxBound]

type MoveSequence = [Move]

isomorphicText :: (Eq t, Show t, Pretty t) => CodeZipper t -> Expectation
isomorphicText cz = plainZipper (toText cz) `isEquivalentTo` cz

isNormalised :: (Eq t, Show t, Pretty t) => CodeZipper t -> Expectation
isNormalised cz = allLines cz `shouldSatisfy` all normalised

normalised :: Eq t => SourceLine t -> Bool
normalised = groupOn tokenType >>> fmap length >>> all (<= 1)

goHome :: (Eq t, Show t, Pretty t) => CodeZipper t -> Expectation
goHome cz = do
    let cz' = home cz
    cz'.tokensBefore `shouldBe` mempty
    cz'.tokensAfter `shouldSatisfy` normalised
    cz'.tokensAfter `shouldStartWith` reverse tokensBeforeWithCurrent
    cz'.linesBefore `shouldBe` cz.linesBefore
    cz'.linesAfter `shouldBe` cz.linesAfter
    home cz' `shouldBe` cz'
  where
    tokensBeforeWithCurrent = case currentToken cz of
        Nothing -> cz.tokensBefore
        Just t -> t : drop 1 cz.tokensBefore

goTop :: (Eq t, Show t, Pretty t) => CodeZipper t -> Expectation
goTop cz = do
    let cz' = top cz
    cz'.linesBefore `shouldBe` mempty
    cz'.linesAfter `shouldBe` maybe [] snd (uncons (allLines cz))
    top cz' `shouldBe` cz'

goMove :: (Eq t, Show t) => Move -> CodeZipper t -> Expectation
goMove m cz = do
    cz' <- tryMove m cz
    position cz' `shouldBe` expectedNewPosition
  where
    position z = (col z, row z)
    (x, y) = position cz
    expectedNewPosition = xyClamp $ case m of
        Up -> (x, y - 1)
        Down -> (x, y + 1)
        Left -> (x - 1, y)
        Right -> (x + 1, y)
        Top -> (x, 0)
        Bottom -> (x, maxBound)
        Home -> (0, y)
        End -> (maxBound, y)
    numberOfLines = length $ allLines cz
    rowClamp = clamp (0, numberOfLines - 1)
    colClamp n = clamp (0, lineWidth $ allLines cz !! n)
    xyClamp (x', y') = let cy = rowClamp y'; cx = colClamp cy x' in (cx, cy)

unchangedByMovement :: (Eq t, Show t) => Move -> CodeZipper t -> Expectation
unchangedByMovement m cz = do
    cz' <- tryMove m cz
    cz' `isEquivalentTo` cz

insertText :: (Eq t, Show t, Pretty t) => CodeZipper t -> Text -> Expectation
insertText cz t = do
    cz' <- within' 100 $ pure $ insert t cz
    toText cz' `shouldContainText` t
    textBefore cz' `shouldBe` (textBefore cz <> t)
    textAfter cz' `shouldBe` textAfter cz

showZipper :: (Eq t, Show t, Pretty t) => CodeZipper t -> String
showZipper cz = unlines [show cz, show [textBefore cz, textAfter cz]]

spec :: Spec
spec = do
    it "does not contain adjacent tokens of the same type" $ property $ isNormalised @Printable
    it "is isomorphic on fromText / toText" $ property $ isomorphicText @Printable
    it "correctly handles home" $ property $ goHome @Printable
    it "correctly handles top" $ property $ goTop @Printable
    it "correctly handles movement" $ property $ goMove @Printable
    it "is unchanged by movement" $ property $ unchangedByMovement @Printable
    it "inserts text correctly" $ property $ insertText @Printable

isEquivalentTo :: (Eq t, Show t) => CodeZipper t -> CodeZipper t -> Expectation
a `isEquivalentTo` b = do
    toText a `shouldBe` toText b
    (a & home & top) `shouldBe` (b & home & top)
    (a & bottom & end) `shouldBe` (b & bottom & end)

shouldContainText :: Text -> Text -> Expectation
a `shouldContainText` b = Text.unpack a `shouldContain` Text.unpack b

shouldStartWithText :: Text -> Text -> Expectation
a `shouldStartWithText` b = Text.unpack a `shouldStartWith` Text.unpack b

shouldEndWithText :: Text -> Text -> Expectation
a `shouldEndWithText` b = Text.unpack a `shouldEndWith` Text.unpack b
