{-# OPTIONS_GHC -fno-warn-orphans #-}

module ACME.Yes.PreCure5.Random
  ( chooseTransformationPhrase
  , chooseMetamorphose
  ) where

import ACME.Yes.PreCure5.Profiles
import System.Random
import Control.Monad.State
import qualified Data.Set as S

instance Random PreCure5 where
  randomR (a, b) g =
    let (p, g') = randomR (fromEnum a, fromEnum b) g
        in (toEnum p, g')
  random = randomR (minBound, maxBound)

chooseTransformationPhrase :: RandomGen g => g -> (String, g)
chooseTransformationPhrase g =
  (flip runState) g $ do
    i <- randomRSt (1, 5)
    p <- randomSt
    return $ transformationPhraseOf $ fiveOrSinglePreCure i p

chooseMetamorphose :: RandomGen g => g -> (String, g)
chooseMetamorphose = chooseTransformationPhrase

fiveOrSinglePreCure :: Int -> PreCure5 -> S.Set PreCure5
fiveOrSinglePreCure 5 _ = allPrecures
fiveOrSinglePreCure _ p = S.singleton p

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt r = state $ randomR r

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random
