{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Sequence.Zipper where

import Control.Applicative ((<|>))
import Data.Sequence (Seq, ViewL (..), ViewR (..), (<|), (|>))
import Data.Sequence qualified as Seq
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)

data SeqZipper t = SeqZipper
    { before :: Seq t
    , after :: Seq t
    }
    deriving stock (Generic, Eq, Show)

empty :: SeqZipper t
empty = SeqZipper{before = Seq.empty, after = Seq.empty}

singleton :: t -> SeqZipper t
singleton x = SeqZipper{before = mempty, after = Seq.singleton x}

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

last :: SeqZipper t -> Maybe t
last SeqZipper{..} = seqLast after <|> seqLast before

forward :: SeqZipper t -> SeqZipper t
forward sz@SeqZipper{..} = case Seq.viewl after of
    EmptyL -> sz
    (x :< after) -> sz{before = before |> x, after}

back :: SeqZipper t -> SeqZipper t
back sz@SeqZipper{..} = case Seq.viewr before of
    EmptyR -> sz
    (before :> x) -> sz{before, after = x <| after}

home :: SeqZipper t -> SeqZipper t
home sz@SeqZipper{..} = sz{before = mempty, after = before <> after}

end :: SeqZipper t -> SeqZipper t
end sz@SeqZipper{..} = sz{before = before <> after, after = mempty}
