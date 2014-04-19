module Main where

import Data.List
import System.Environment
import System.Random
import qualified Data.Set as S

import qualified ACME.Yes.PreCure5 as Yes
import ACME.Yes.PreCure5.Class (allPrecures)
import qualified ACME.Yes.PreCure5.Random as R
import qualified ACME.Yes.PreCure5.GoGo as GoGo

main :: IO ()
main = do
  arg <- intercalate " " `fmap` getArgs
  g <- getStdGen
  putStr $ possibblyInfiniteMetamorphoses g arg

possibblyInfiniteMetamorphoses :: (RandomGen g) => g -> String -> String
possibblyInfiniteMetamorphoses g = unlines . repeat . possiblyMetamorphose g

possiblyMetamorphose :: (RandomGen g) => g -> String -> String
possiblyMetamorphose g s
  | GoGo.isPreCure5GoGo s = metamorphoseGoGo
  | Yes.isPreCure5 s = metamorphose
  | otherwise = generateLine s
  where
    (metamorphoseGoGo, _) = R.chooseMetamorphoseFrom (allPrecures :: S.Set GoGo.PreCure5GoGo) g
    (metamorphose    , _) = R.chooseMetamorphoseFrom (allPrecures :: S.Set Yes.PreCure5     ) g

generateLine :: String -> String
generateLine "" = "y"
generateLine s  = s
