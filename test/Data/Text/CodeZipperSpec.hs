{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Text.CodeZipperSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (race)
import Control.Exception (AssertionFailed (..), throw)
import Control.Monad (foldM_)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.Either.Extra (fromEither)
import Data.Foldable (foldrM)
import Data.Function ((&))
import Data.List (uncons)
import Data.Ord (clamp)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.CodeZipper
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (Left, Right)

data Printable = Graphic | Whitespace
    deriving (Eq, Show)

type PrintableToken = Token Printable

type PrintableLine = SourceLine Printable

instance Pretty Printable where
    prettify _ = fmap onSpace . Text.split (== '\n')
      where
        onSpace "" = []
        onSpace t = case Text.break isSpace t of
            ("", rt) -> onGraph rt
            (t', rt) -> (Graphic, t') : onGraph rt
        onGraph "" = []
        onGraph t = case Text.break (not . isSpace) t of
            ("", rt) -> onSpace rt
            (t', rt) -> (Whitespace, t') : onSpace rt

instance Arbitrary Text where
    arbitrary = Text.pack <$> listOf (elements $ ['a' .. 'z'] <> ['A' .. 'Z'] <> [' ', '\t', '\n'])

instance Arbitrary (CodeZipper Printable) where
    arbitrary = fromText "" <$> arbitrary

data Move = Up | Down | Left | Right | Home | End | Top | Bottom
    deriving (Bounded, Enum, Eq, Show)

within' :: Int -> IO a -> IO a
within' ms e = do
    winner <- race e $ do
        liftIO $ threadDelay ms
        throw $ AssertionFailed "Timeout exceeded"
    pure $ fromEither winner

tryMove :: (Eq t, Show t) => Move -> CodeZipper t -> IO (CodeZipper t)
tryMove m cz = within' 100 $ pure $ move m cz

move :: (Eq t, Show t) => Move -> CodeZipper t -> CodeZipper t
move Up = up
move Down = down
move Left = left
move Right = right
move Home = home
move End = end
move Top = top
move Bottom = bottom

instance Arbitrary Move where arbitrary = elements [minBound .. maxBound]

type MoveSequence = [Move]

isomorphicText :: CodeZipper Printable -> Expectation
isomorphicText cz = fromText "" (toText cz) `isEquivalentTo` cz

goHome :: CodeZipper Printable -> Expectation
goHome cz = do
    let homed = home cz
    homed.tokensBefore `shouldBe` mempty
    homed.tokensAfter `shouldStartWith` reverse cz.tokensBefore
    homed.linesBefore `shouldBe` cz.linesBefore
    homed.linesAfter `shouldBe` cz.linesAfter
    home homed `shouldBe` homed

goTop :: CodeZipper Printable -> Expectation
goTop cz = do
    let topped = top cz
    topped.linesBefore `shouldBe` mempty
    topped.linesAfter `shouldBe` maybe [] snd (uncons (allLines cz))
    top topped `shouldBe` topped

goMove :: CodeZipper Printable -> MoveSequence -> Expectation
goMove = foldM_ go . (0,0,)
  where
    go :: (Int, Int, CodeZipper Printable) -> Move -> IO (Int, Int, CodeZipper Printable)
    go (x, y, cz) m = do
        row cz `shouldBe` y
        col cz `shouldBe` x
        let numberOfLines = length (allLines cz)
        let rowClamp = clamp (0, numberOfLines - 1)
        let colClamp n = clamp (0, lineWidth $ allLines cz !! n)
        let xyClamp (x', y') = let cy = rowClamp y'; cx = colClamp cy x' in (cx, cy)
        cz' <- tryMove m cz
        let (x', y') = xyClamp $ case m of
                Up -> (x, y - 1)
                Down -> (x, y + 1)
                Left -> (x - 1, y)
                Right -> (x + 1, y)
                Top -> (x, 0)
                Bottom -> (x, maxBound)
                Home -> (0, y)
                End -> (maxBound, y)
        row cz' `shouldBe` y'
        col cz' `shouldBe` x'
        pure (x', y', cz')

unchangedByMovement :: CodeZipper Printable -> MoveSequence -> Expectation
unchangedByMovement cz s = do
    moved :: CodeZipper Printable <- liftIO $ foldrM tryMove cz s
    moved `isEquivalentTo` cz

insertText :: CodeZipper Printable -> MoveSequence -> Text -> Expectation
insertText cz s t = do
    moved :: CodeZipper Printable <- liftIO $ foldrM tryMove cz s
    inserted :: CodeZipper Printable <- within' 100 $ pure $ insert t moved
    toText inserted `shouldContainText` t
    textBefore inserted `shouldBe` (textBefore moved <> t)
    textAfter inserted `shouldBe` textAfter moved

showZipper :: (Eq t, Show t) => CodeZipper t -> String
showZipper cz = unlines [show cz, show [textBefore cz, textAfter cz]]

spec :: Spec
spec = describe "CodeZipper" $ do
    it "is isomorphic on fromText / toText" $ property isomorphicText
    it "correctly handles home" $ property goHome
    it "correctly handles top" $ property goTop
    it "correctly handles movement" $ property goMove
    it "is unchanged by movement" $ property unchangedByMovement
    it "inserts text correctly" $ property insertText

isEquivalentTo :: (Eq t, Show t) => CodeZipper t -> CodeZipper t -> Expectation
a `isEquivalentTo` b = within' 100 $ do
    toText a `shouldBe` toText b
    (a & home & top) `shouldBe` (b & home & top)
    (a & bottom & end) `shouldBe` (b & bottom & end)

shouldContainText :: Text -> Text -> Expectation
a `shouldContainText` b = Text.unpack a `shouldContain` Text.unpack b

shouldEndWithText :: Text -> Text -> Expectation
a `shouldEndWithText` b = Text.unpack a `shouldEndWith` Text.unpack b
