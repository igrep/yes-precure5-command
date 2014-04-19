{-# OPTIONS_GHC -fno-warn-orphans #-}

module ACME.Yes.PreCure5.Random
  ( choosePrecureFrom
  , chooseTransformationPhraseFrom
  , chooseMetamorphoseFrom
  ) where

import ACME.Yes.PreCure5.Class
import System.Random
import Control.Monad.State
import qualified Data.Set as S

choosePrecureFrom :: (PreCure p, RandomGen g) => S.Set p -> g -> (p, g)
choosePrecureFrom pset g = (ps !! i, g')
  where
    ps = S.toAscList pset
    (i, g') = randomR (0, l) g
    l = S.size pset - 1

chooseTransformationPhraseFrom :: (PreCure p, RandomGen g) => S.Set p -> g -> (String, g)
chooseTransformationPhraseFrom pset g =
  (flip runState) g $ do
    i <- randomRSt (1, 5)
    p <- state $ choosePrecureFrom pset
    return $ transformationPhraseOf $ fiveOrSinglePreCure i p

chooseMetamorphoseFrom :: (PreCure p, RandomGen g) => S.Set p -> g -> (String, g)
chooseMetamorphoseFrom = chooseTransformationPhraseFrom

fiveOrSinglePreCure :: (PreCure p) => Int -> p -> S.Set p
fiveOrSinglePreCure 5 _ = allPrecures
fiveOrSinglePreCure _ p = S.singleton p

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt r = state $ randomR r
