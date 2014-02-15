{-# OPTIONS_GHC -fno-warn-orphans #-}

module ACME.Yes.PreCure5.Random
  ( choosePreCure
  , chooseTransformationPhrase
  , chooseMetamorphose
  ) where

import ACME.Yes.PreCure5.Class
import System.Random
import Control.Monad.State
import qualified Data.Set as S

choosePrecure :: (RandomGen g, PreCure p) => g -> (p, g)
choosePrecure g = (ps !! i, g')
  where
    pset = allPrecures
    ps = S.toAscList pset
    (i, g') = randomR (0, l) g
    l = S.size pset - 1

chooseTransformationPhrase :: RandomGen g => g -> (String, g)
chooseTransformationPhrase g =
  (flip runState) g $ do
    i <- randomRSt (1, 5)
    p <- state choosePrecure
    return $ transformationPhraseOf $ fiveOrSinglePreCure i p

chooseMetamorphose :: RandomGen g => g -> (String, g)
chooseMetamorphose = chooseTransformationPhrase

fiveOrSinglePreCure :: (PreCure p) => Int -> p -> S.Set p
fiveOrSinglePreCure 5 _ = allPrecures
fiveOrSinglePreCure _ p = S.singleton p

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt r = state $ randomR r
