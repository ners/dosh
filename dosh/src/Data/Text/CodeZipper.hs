{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Text.CodeZipper where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find, uncons)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Prelude

data Token t = Token
    { tokenType :: t
    , tokenContent :: Text
    }
    deriving stock (Generic, Eq)

instance Show t => Show (Token t) where
    show Token{..} = show tokenType <> " " <> show tokenContent

type SourceLine t = [Token t]

class Pretty t where
    plain :: Text -> [SourceLine t]
    pretty :: Text -> Text -> Maybe [SourceLine t]

plainLine :: Pretty t => Text -> SourceLine t
plainLine = maybe [] fst . uncons . plain

data CodeZipper t = CodeZipper
    { language :: Text
    , linesBefore :: [SourceLine t]
    , linesAfter :: [SourceLine t]
    , tokensBefore :: [Token t]
    , tokensAfter :: [Token t]
    }
    deriving stock (Generic, Eq, Show)

instance Eq t => Monoid (CodeZipper t) where
    mempty = empty

instance Eq t => Semigroup (CodeZipper t) where
    a <> b = a{linesAfter = linesAfter a <> allLines b}

empty :: CodeZipper t
empty =
    CodeZipper
        { language = mempty
        , linesBefore = mempty
        , linesAfter = mempty
        , tokensBefore = mempty
        , tokensAfter = mempty
        }

plainZipper :: Pretty t => Text -> Text -> CodeZipper t
plainZipper before after = empty{linesBefore, linesAfter, tokensBefore, tokensAfter}
  where
    (reverse -> tokensBefore, linesBefore) = fromMaybe ([], []) $ uncons $ reverse $ plain before
    (tokensAfter, linesAfter) = fromMaybe ([], []) $ uncons $ plain after

prettyZipper :: Pretty t => Text -> Text -> Maybe (CodeZipper t)
prettyZipper language t = pretty language t <&> \(fromMaybe ([], []) . uncons -> (tokensAfter, linesAfter)) -> empty{language, tokensAfter, linesAfter}

currentLine :: Eq t => CodeZipper t -> SourceLine t
currentLine CodeZipper{tokensBefore, tokensAfter} =
    normaliseToks $ reverse tokensBefore <> tokensAfter

null :: CodeZipper t -> Bool
null CodeZipper{..} = no linesBefore && no linesAfter && no tokensBefore && no tokensAfter
  where
    no :: [a] -> Bool
    no = Prelude.null

lines :: CodeZipper t -> Int
lines CodeZipper{linesBefore, linesAfter} = length linesBefore + 1 + length linesAfter

allLines :: Eq t => CodeZipper t -> [SourceLine t]
allLines cz@CodeZipper{linesBefore, linesAfter} =
    reverse linesBefore <> [currentLine cz] <> linesAfter

row :: CodeZipper t -> Int
row = length . linesBefore

col :: CodeZipper t -> Int
col = lineWidth . tokensBefore

currentToken :: Eq t => CodeZipper t -> Maybe (Token t)
currentToken CodeZipper{tokensBefore = []} = Nothing
currentToken CodeZipper{tokensBefore = tb : _, tokensAfter = []} = Just tb
currentToken CodeZipper{tokensBefore = tb : _, tokensAfter = ta : _} = Just $ maybe tb fst (uncons (normaliseToks [tb, ta]))

nextChar :: CodeZipper t -> Maybe Char
nextChar CodeZipper{tokensAfter = [], linesAfter = []} = Nothing
nextChar CodeZipper{tokensAfter} = maybe (Just '\n') (Just . Text.head . tokenContent) $ find ((> 0) . tokenWidth) tokensAfter

prevChar :: CodeZipper t -> Maybe Char
prevChar CodeZipper{tokensBefore = [], linesBefore = []} = Nothing
prevChar CodeZipper{tokensBefore} = maybe (Just '\n') (Just . Text.last . tokenContent) $ find ((> 0) . tokenWidth) tokensBefore

textBefore :: CodeZipper t -> Text
textBefore CodeZipper{linesBefore, tokensBefore} =
    Text.unlines (lineToText <$> reverse linesBefore) <> lineToText (reverse tokensBefore)

textAfter :: CodeZipper t -> Text
textAfter CodeZipper{linesAfter, tokensAfter} =
    Text.intercalate "\n" $ lineToText <$> (tokensAfter : linesAfter)

toText :: CodeZipper t -> Text
toText cz = textBefore cz <> textAfter cz

lineToText :: SourceLine t -> Text
lineToText line = mconcat $ tokenContent <$> line

insert :: (Eq t, Pretty t) => Text -> CodeZipper t -> CodeZipper t
insert t cz@CodeZipper{..} =
    cz
        { tokensAfter = NonEmpty.head insertedLines'
        , linesAfter = NonEmpty.tail insertedLines' <> linesAfter
        }
        & downN (NonEmpty.length insertedLines - 1)
        & (if length insertedLines > 1 then home else id)
        & rightN (lineWidth $ NonEmpty.last insertedLines)
        & prettifyZipper
  where
    insertedLines = uncurry (:|) $ fromMaybe ([], []) $ uncons (plain t)
    insertedLines' = insertedLines & overLast (<> tokensAfter)

prettifyZipper :: (Eq t, Pretty t) => CodeZipper t -> CodeZipper t
prettifyZipper cz@CodeZipper{language} =
    maybe cz (rightN (col cz) . downN (row cz)) $
        prettyZipper language $
            toText cz

overLast :: (a -> a) -> NonEmpty a -> NonEmpty a
overLast f = NonEmpty.reverse . (\(x :| xs) -> f x :| xs) . NonEmpty.reverse

insertChar :: (Eq t, Pretty t) => Char -> CodeZipper t -> CodeZipper t
insertChar = insert . Text.singleton

left :: Eq t => CodeZipper t -> CodeZipper t
left cz@CodeZipper{tokensBefore = []} = cz
left cz@CodeZipper{tokensBefore = tb : tbs, tokensAfter} =
    cz
        { tokensBefore = normaliseToks $ tb' : tbs
        , tokensAfter = normaliseToks $ ta' : tokensAfter
        }
  where
    (tb', ta') = splitTokenAt (tokenWidth tb - 1) tb

leftN :: Eq t => Int -> CodeZipper t -> CodeZipper t
leftN n cz
    | n < 1 = cz
    | otherwise = iterate left cz !! n

right :: Eq t => CodeZipper t -> CodeZipper t
right cz@CodeZipper{tokensAfter = []} = cz
right cz@CodeZipper{tokensAfter = ta : tas, tokensBefore} =
    cz
        { tokensBefore = reverse $ normaliseToks $ reverse $ tb' : tokensBefore
        , tokensAfter = normaliseToks $ ta' : tas
        }
  where
    (tb', ta') = splitTokenAt 1 ta

rightN :: Eq t => Int -> CodeZipper t -> CodeZipper t
rightN n cz
    | n < 1 = cz
    | otherwise = iterate right cz !! n

-- | Go to the beginning of the current line.
home :: Eq t => CodeZipper t -> CodeZipper t
home cz =
    cz
        { tokensBefore = []
        , tokensAfter = currentLine cz
        }

-- | Go to the end of the current line.
end :: Eq t => CodeZipper t -> CodeZipper t
end cz =
    cz
        { tokensAfter = []
        , tokensBefore = reverse $ currentLine cz
        }

deleteLeft :: (Eq t, Pretty t) => CodeZipper t -> CodeZipper t
deleteLeft cz@CodeZipper{tokensBefore = [], linesBefore = []} = cz
deleteLeft cz@CodeZipper{tokensBefore = [], linesBefore = lb : lbs} =
    cz
        { linesBefore = lbs
        , tokensBefore = reverse lb
        }
        & prettifyZipper
deleteLeft cz@CodeZipper{tokensBefore = tb : tbs} =
    cz
        { tokensBefore = normaliseToks $ tb' : tbs
        }
        & prettifyZipper
  where
    (tb', _) = splitTokenAt (tokenWidth tb - 1) tb

deleteRight :: (Eq t, Pretty t) => CodeZipper t -> CodeZipper t
deleteRight cz@CodeZipper{tokensAfter = [], linesAfter = []} = cz
deleteRight cz@CodeZipper{tokensAfter = [], linesAfter = la : las} =
    cz
        { linesAfter = las
        , tokensAfter = la
        }
        & prettifyZipper
deleteRight cz@CodeZipper{tokensAfter = ta : tas} =
    cz
        { tokensAfter = normaliseToks $ ta' : tas
        }
        & prettifyZipper
  where
    (_, ta') = splitTokenAt 1 ta

-- | Go up to the previous line and try to preserve the current column.
up :: Eq t => CodeZipper t -> CodeZipper t
up cz@CodeZipper{linesBefore = []} = cz
up cz@CodeZipper{tokensBefore} = up' cz & rightN (lineWidth tokensBefore)

-- | Go up to the beginning of the previous line.
up' :: Eq t => CodeZipper t -> CodeZipper t
up' cz@CodeZipper{linesBefore = []} = cz
up' cz@CodeZipper{linesBefore = lb : lbs, linesAfter} =
    cz
        { linesBefore = lbs
        , linesAfter = normaliseToks (currentLine cz) : linesAfter
        , tokensBefore = []
        , tokensAfter = lb
        }

-- | Go up N lines and try to preserve the current column.
upN :: Eq t => Int -> CodeZipper t -> CodeZipper t
upN _ cz@CodeZipper{linesBefore = []} = cz
upN n cz@CodeZipper{tokensBefore}
    | n < 1 = cz
    | otherwise =
        (iterate up' cz !! n)
            & rightN (lineWidth tokensBefore)

-- | Go down to the next line and try to preserve the current column.
down :: Eq t => CodeZipper t -> CodeZipper t
down cz@CodeZipper{linesAfter = []} = cz
down cz@CodeZipper{tokensBefore} = cz & down' & rightN (lineWidth tokensBefore)

-- | Go down to the beginning of the next line.
down' :: Eq t => CodeZipper t -> CodeZipper t
down' cz@CodeZipper{linesAfter = []} = cz
down' cz@CodeZipper{linesAfter = la : las, linesBefore} =
    cz
        { linesAfter = las
        , linesBefore = currentLine cz : linesBefore
        , tokensBefore = []
        , tokensAfter = la
        }

-- | Go down N lines and try to preserve the current column.
downN :: Eq t => Int -> CodeZipper t -> CodeZipper t
downN _ cz@CodeZipper{linesAfter = []} = cz
downN n cz@CodeZipper{tokensBefore}
    | n < 1 = cz
    | otherwise =
        (iterate down' cz !! n)
            & rightN (lineWidth tokensBefore)

-- | Go to the first line and try to preserve the current column.
top :: Eq t => CodeZipper t -> CodeZipper t
top cz@CodeZipper{linesBefore} = length linesBefore `upN` cz

-- | Go to the last line and try to preserve the current column.
bottom :: Eq t => CodeZipper t -> CodeZipper t
bottom cz@CodeZipper{linesAfter} = length linesAfter `downN` cz

mapTokenContent :: (Text -> Text) -> Token t -> Token t
mapTokenContent f t = t{tokenContent = f t.tokenContent}

tokenWidth :: Token t -> Int
tokenWidth = Text.length . tokenContent

nullToken :: Token t -> Bool
nullToken = Text.null . tokenContent

append :: Text -> Token t -> Token t
append t = mapTokenContent (<> t)

prepend :: Text -> Token t -> Token t
prepend t = mapTokenContent (t <>)

lineWidth :: SourceLine t -> Int
lineWidth = sum . fmap tokenWidth

splitTokenAt :: Int -> Token t -> (Token t, Token t)
splitTokenAt i t | i < 0 = splitTokenAt 0 t
splitTokenAt i t = (t{tokenContent = a}, t{tokenContent = b})
  where
    (a, b) = Text.splitAt i t.tokenContent

splitTokenAt' :: Int -> Token t -> (Token t, Token t, Token t)
splitTokenAt' i t = (a, b, c')
  where
    (a, c) = splitTokenAt i t
    (b, c') = splitTokenAt 1 c

normaliseToks :: forall t. Eq t => [Token t] -> [Token t]
normaliseToks = foldr maybePrepend []
  where
    maybePrepend :: Token t -> [Token t] -> [Token t]
    maybePrepend (nullToken -> True) acc = acc
    maybePrepend t (p : ps) | t.tokenType == p.tokenType = prepend t.tokenContent p : ps
    maybePrepend t ps = t : ps

tokenLines :: Token t -> [Token t]
tokenLines t = Text.splitOn "\n" t.tokenContent <&> \c -> t{tokenContent = c}
