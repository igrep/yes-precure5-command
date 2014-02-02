module Main where

import Data.List
import System.Environment
import System.Random

import ACME.Yes.PreCure5

main :: IO ()
main = do
  arg <- intercalate " " `fmap` getArgs
  g <- getStdGen
  putStr $ possibblyInfiniteMetamorphoses g arg

possibblyInfiniteMetamorphoses :: (RandomGen g) => g -> String -> String
possibblyInfiniteMetamorphoses g = unlines . repeat . possiblyMetamorphose g

possiblyMetamorphose :: (RandomGen g) => g -> String -> String
possiblyMetamorphose g s =
  if isPreCure5 s
    then metamorphose ++ "\n"
    else generateLine s
  where (metamorphose, _) = chooseTransformationPhrase g

generateLine :: String -> String
generateLine "" = "y"
generateLine s  = s
