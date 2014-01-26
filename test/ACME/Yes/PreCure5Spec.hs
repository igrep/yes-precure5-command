module ACME.Yes.PreCure5Spec where

import ACME.Yes.PreCure5
import Test.Hspec
import Control.Monad

spec :: Spec
spec = do
  describe "isPreCure5" $ do
    it "is not to be blank" $
      "" `shouldSatisfy` (not . isPreCure5)

    forM_ ["プリキュア5", "プリキュア５", "PreCure 5"] itIsPreCure5

itIsPreCure5 :: String -> Spec
itIsPreCure5 s =
  it ("is to be " ++ s) $ s `shouldSatisfy` isPreCure5
