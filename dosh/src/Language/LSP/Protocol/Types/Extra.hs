module Language.LSP.Protocol.Types.Extra where

import Data.Row
import Language.LSP.Protocol.Types
import Prelude

partialTextDocumentContentChangeEvent
    :: Range
    -> Maybe UInt
    -> Text
    -> TextDocumentContentChangeEvent
partialTextDocumentContentChangeEvent range rangeLength text =
    TextDocumentContentChangeEvent . InL $
        #range .== range .+ #rangeLength .== rangeLength .+ #text .== text
