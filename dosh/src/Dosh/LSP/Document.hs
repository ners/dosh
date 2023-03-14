module Dosh.LSP.Document where

import Data.Sequence.Zipper (SeqZipper, backWhile, forwardWhile)
import Data.UUID (UUID)
import Development.IDE (Diagnostic, Uri)
import Dosh.Prelude
import Language.LSP.Types (CompletionItem)

data ChunkType = Module | Declaration | Expression
    deriving stock (Show, Eq)

data ChunkMetadata = ChunkMetadata
    { cellId :: UUID
    , chunkIndex :: Int
    , chunkType :: ChunkType
    , firstLine :: Int
    , lastLine :: Int
    }
    deriving stock (Generic, Show)

data Document = Document
    { uri :: Uri
    , language :: Text
    , chunks :: SeqZipper ChunkMetadata
    , contents :: Text
    , diagnostics :: [Diagnostic]
    , completions :: [CompletionItem]
    }
    deriving stock (Generic, Show)

newDocument :: Uri -> Document
newDocument uri =
    Document
        { uri
        , language = mempty
        , chunks = mempty
        , contents = mempty
        , diagnostics = mempty
        , completions = mempty
        }

goToLine :: Int -> Document -> Document
goToLine r doc = doc{chunks = (lowerBound . upperBound) doc.chunks}
  where
    upperBound = forwardWhile chunkContains
    lowerBound = backWhile chunkContains
    chunkContains ChunkMetadata{..} = r >= firstLine && r <= lastLine
