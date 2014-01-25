module Main where

import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStr $ generateInfinite args

generateInfinite :: [String] -> String
generateInfinite = unlines . repeat . generateLine

generateLine :: [String] -> String
generateLine [] = "y"
generateLine xs = intercalate " " xs
