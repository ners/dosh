module Data.Text.CodeZipper where

import Data.Function ((&))
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

type Token t = (t, Text)

type SourceLine t = [Token t]

class Pretty t where
    prettify :: Text -> Text -> [SourceLine t]

data CodeZipper t = CodeZipper
    { language :: Text
    , linesBefore :: [SourceLine t]
    , linesAfter :: [SourceLine t]
    , tokensBefore :: [Token t]
    , tokensAfter :: [Token t]
    }
    deriving (Eq, Show)

fromText :: Pretty t => Text -> Text -> CodeZipper t
fromText language t =
    CodeZipper
        { language
        , linesBefore = []
        , linesAfter
        , tokensBefore = []
        , tokensAfter
        }
  where
    (tokensAfter, linesAfter) = fromMaybe ([], []) $ uncons (prettify language t)

currentLine :: Eq t => CodeZipper t -> SourceLine t
currentLine CodeZipper{tokensBefore, tokensAfter} =
    normaliseToks $ reverse tokensBefore <> tokensAfter

allLines :: Eq t => CodeZipper t -> [SourceLine t]
allLines cz@CodeZipper{linesBefore, linesAfter} =
    reverse linesBefore <> [currentLine cz] <> linesAfter

currentToken :: Eq t => CodeZipper t -> Maybe (Token t)
currentToken CodeZipper{tokensBefore = []} = Nothing
currentToken CodeZipper{tokensBefore = tb : _, tokensAfter = []} = Just tb
currentToken CodeZipper{tokensBefore = tb : _, tokensAfter = ta : _} = Just $ maybe tb fst (uncons (normaliseToks [tb, ta]))

textBefore :: CodeZipper t -> Text
textBefore CodeZipper{linesBefore, tokensBefore} =
    Text.unlines (lineToText <$> reverse linesBefore) <> lineToText (reverse tokensBefore)

textAfter :: CodeZipper t -> Text
textAfter CodeZipper{linesAfter, tokensAfter} =
    Text.intercalate "\n" $ lineToText <$> (tokensAfter : linesAfter)

toText :: Eq t => CodeZipper t -> Text
toText cz = textBefore cz <> textAfter cz

tokenToText :: Token t -> Text
tokenToText = snd

lineToText :: SourceLine t -> Text
lineToText line = mconcat $ tokenToText <$> line

insert :: (Eq t, Pretty t) => Text -> CodeZipper t -> CodeZipper t
insert t cz@CodeZipper{..} =
    cz
        { tokensAfter = NonEmpty.head insertedLines'
        , linesAfter = reverse (NonEmpty.tail insertedLines') <> linesAfter
        }
        & downN (NonEmpty.length insertedLines - 1)
        & (if length insertedLines > 1 then home else id)
        & rightN (lineWidth $ NonEmpty.last insertedLines)
        & prettifyZipper
  where
    insertedLines = uncurry (:|) $ fromMaybe ([], []) $ uncons (prettify language t)
    insertedLines' = insertedLines & overLast (<> tokensAfter)

prettifyZipper :: (Eq t, Pretty t) => CodeZipper t -> CodeZipper t
prettifyZipper cz@CodeZipper{language, linesBefore, tokensBefore} =
    toText cz
        & fromText language
        & downN (length linesBefore)
        & rightN (lineWidth tokensBefore)

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

deleteLeft :: Eq t => CodeZipper t -> CodeZipper t
deleteLeft cz@CodeZipper{tokensBefore = []} = cz
deleteLeft cz@CodeZipper{tokensBefore = tb : tbs} =
    cz
        { tokensBefore = normaliseToks $ tb' : tbs
        }
  where
    (tb', _) = splitTokenAt (tokenWidth tb - 1) tb

deleteRight :: Eq t => CodeZipper t -> CodeZipper t
deleteRight cz@CodeZipper{tokensAfter = []} = cz
deleteRight cz@CodeZipper{tokensAfter = ta : tas} =
    cz
        { tokensAfter = normaliseToks $ ta' : tas
        }
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
        , linesBefore = normaliseToks (currentLine cz) : linesBefore
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

tokenWidth :: Token t -> Int
tokenWidth = Text.length . snd

lineWidth :: SourceLine t -> Int
lineWidth = sum . fmap tokenWidth

splitTokenAt :: Int -> Token t -> (Token t, Token t)
splitTokenAt i t | i < 0 = splitTokenAt 0 t
splitTokenAt i (tokenType, tokenText) = ((tokenType, a), (tokenType, b))
  where
    (a, b) = Text.splitAt i tokenText

splitTokenAt' :: Int -> Token t -> (Token t, Token t, Token t)
splitTokenAt' i t = (a, b, c')
  where
    (a, c) = splitTokenAt i t
    (b, c') = splitTokenAt 1 c

normaliseToks :: Eq t => [Token t] -> [Token t]
normaliseToks [t] = [t]
normaliseToks ts = foldr prepend [] ts
  where
    prepend (_, t) acc | Text.null t = acc
    prepend t [] = [t]
    prepend t@(ttype, ttext) ps@((ptype, ptext) : pps)
        | ttype == ptype = (ptype, ttext <> ptext) : pps
        | otherwise = t : ps
