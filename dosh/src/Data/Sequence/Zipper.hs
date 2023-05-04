{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Sequence.Zipper where

import Control.Applicative ((<|>))
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Sequence (Seq, ViewL (..), ViewR (..), (<|), (|>))
import Data.Sequence qualified as Seq
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import Prelude

data SeqZipper t = SeqZipper
    { before :: Seq t
    , after :: Seq t
    }
    deriving stock (Generic, Eq, Show)

empty :: SeqZipper t
empty = SeqZipper{before = Seq.empty, after = Seq.empty}

singleton :: t -> SeqZipper t
singleton x = SeqZipper{before = mempty, after = Seq.singleton x}

length :: SeqZipper t -> Int
length SeqZipper{..} = Seq.length before + Seq.length after

instance Semigroup (SeqZipper t) where
    a <> b = SeqZipper{before = before a, after = after a <> before b <> after b}

instance Monoid (SeqZipper t) where
    mempty = empty

instance IsList (SeqZipper t) where
    type Item (SeqZipper t) = t
    fromList xs = SeqZipper{before = mempty, after = fromList xs}
    toList SeqZipper{..} = toList $ before <> after

instance Foldable SeqZipper where
    foldr f z = foldr f z . toList

instance Traversable SeqZipper where
    traverse :: Applicative f => (a -> f b) -> SeqZipper a -> f (SeqZipper b)
    traverse f SeqZipper{..} = do
        before <- traverse f before
        after <- traverse f after
        pure SeqZipper{..}

instance Functor SeqZipper where
    fmap :: (a -> b) -> SeqZipper a -> SeqZipper b
    fmap f SeqZipper{..} =
        SeqZipper
            { before = fmap f before
            , after = fmap f after
            }

seqFirst :: Seq a -> Maybe a
seqFirst s = case Seq.viewl s of
    EmptyL -> Nothing
    (x :< _) -> Just x

seqLast :: Seq a -> Maybe a
seqLast s = case Seq.viewr s of
    EmptyR -> Nothing
    (_ :> x) -> Just x

first :: SeqZipper t -> Maybe t
first SeqZipper{..} = seqFirst before

current :: SeqZipper t -> Maybe t
current SeqZipper{..} = seqFirst after

next :: SeqZipper t -> Maybe t
next = current . forward

previous :: SeqZipper t -> Maybe t
previous SeqZipper{before} = case Seq.viewr before of
    EmptyR -> Nothing
    (_ :> x) -> Just x

last :: SeqZipper t -> Maybe t
last SeqZipper{..} = seqLast after <|> seqLast before

forward :: SeqZipper t -> SeqZipper t
forward sz@SeqZipper{..} = case Seq.viewl after of
    EmptyL -> sz
    (x :< after) -> sz{before = before |> x, after}

forwardWhile :: (t -> Bool) -> SeqZipper t -> SeqZipper t
forwardWhile p = fromJust . find (not . maybe False p . current) . iterate forward

back :: SeqZipper t -> SeqZipper t
back sz@SeqZipper{..} = case Seq.viewr before of
    EmptyR -> sz
    (before :> x) -> sz{before, after = x <| after}

backWhile :: (t -> Bool) -> SeqZipper t -> SeqZipper t
backWhile p sz = go $ Prelude.take (1 + Seq.length (before sz)) $ iterate back sz
  where
    go [x] = x
    go (x : y : xs)
        | maybe True p (current y) = go (y : xs)
        | otherwise = x
    go _ = sz

home :: SeqZipper t -> SeqZipper t
home sz@SeqZipper{..} = sz{before = mempty, after = before <> after}

end :: SeqZipper t -> SeqZipper t
end sz@SeqZipper{..} = sz{before = before <> after, after = mempty}

drop :: Int -> SeqZipper t -> SeqZipper t
drop n sz@SeqZipper{..} = sz{after = Seq.drop n after}

dropBefore :: Int -> SeqZipper t -> SeqZipper t
dropBefore n sz@SeqZipper{..} = Data.Sequence.Zipper.drop n' $ iterate back sz !! n'
  where
    n' = min n $ Seq.length before

take :: Int -> SeqZipper t -> Seq t
take n SeqZipper{..} = Seq.take n after

takeBefore :: Int -> SeqZipper t -> Seq t
takeBefore n sz@SeqZipper{..} = Data.Sequence.Zipper.take n' $ iterate back sz !! n'
  where
    n' = min n $ Seq.length before

-- | Insert a new element before the current element (after the cursor) and move the focus to the newly inserted one.
insert :: t -> SeqZipper t -> SeqZipper t
insert v sz@SeqZipper{..} = sz{after = v <| after}

{- | Insert a new element after the current element, keeping the focus on the current element.
 In absence of a current element (at the end of a sequence), the inserted element becomes the current element.
-}
insertAfter :: t -> SeqZipper t -> SeqZipper t
insertAfter v sz@SeqZipper{..}
    | Seq.null after = insert v sz
    | otherwise = back $ insert v $ forward sz

-- | Insert a new element before the current element and keep the focus unchanged.
insertBefore :: t -> SeqZipper t -> SeqZipper t
insertBefore v sz@SeqZipper{..} = sz{before = before |> v}
