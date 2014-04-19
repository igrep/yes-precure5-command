{-# OPTIONS_GHC -fno-warn-orphans #-}
module ACME.Yes.PreCure5.GoGo.ProfilesSpec where

import ACME.Yes.PreCure5.GoGo.Profiles
import qualified ACME.Yes.PreCure5.Profiles as Yes

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import qualified Data.Set as S

import Debug.Trace
import Control.Monad

traceId :: String -> String
traceId = join trace

instance Arbitrary Yes.PreCure5 where
  arbitrary = elements $ S.toList allPrecures

spec :: Spec
spec = do
  describe "transformationPhraseOf" $ do
    prop "is same phrase as `transformationPhraseOf PreCure5`" $
      \yes ->
        let gogo = PreCure5 yes in
        Yes.transformationPhraseOf (S.singleton yes) == transformationPhraseOf (S.singleton gogo)

    prop "is PreCure5's phrase with MilkyRose's phrase" $
      \yes ->
        let gogo = PreCure5 yes in
        transformationPhraseOf (S.singleton $ gogo) ++ transformationPhraseOf (S.singleton MilkyRose)
          == transformationPhraseOf (S.fromList [gogo, MilkyRose])
