module Main (main) where

import Lib
import Data.Text.IO as T ( getLine )

main :: IO ()
main = do
  t <- T.getLine
  case parseUri t of
    Left err -> putStrLn $ "Error: " ++ show err
    Right uri -> do
      putStrLn $ "Parsed URI: " ++ show uri
