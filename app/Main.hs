module Main where

import MyLib qualified (someFunc)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    MyLib.someFunc
