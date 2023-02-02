module Main where

import MyLib qualified (someFunc)

main :: IO ()
main = MyLib.someFunc
