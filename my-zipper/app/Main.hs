module Main where

import MyLib qualified (go, output)

main :: IO ()
main = do
  MyLib.go
