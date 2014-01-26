module ACME.Yes.PreCure5Spec where

import ACME.Yes.PreCure5
import Test.Hspec

spec :: Spec
spec = do
  describe "isPreCure5" $ do
    it "is not blank" $
      "" `shouldSatisfy` (not . isPreCure5)
