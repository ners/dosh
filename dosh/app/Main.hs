module Main where

import Dosh.CodeInput
import Dosh.LSP.Session
import Dosh.Prelude

main :: IO ()
main = runSession do
    void codeInput
