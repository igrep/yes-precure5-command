module ACME.Yes.PreCure5.ParserSpec where

import ACME.Yes.PreCure5.Parser
import Test.Hspec
import Control.Monad

spec :: Spec
spec = do
  describe "isPreCure5" $ do
    it "is not to be blank" $
      "" `shouldSatisfy` (not . isPreCure5)

    forM_ ["プリキュア5", "プリキュア５", "PreCure 5", "Precure 5", "precure 5", "PRECURE 5"] itIsPreCure5

    forM_ ["Yes! PreCure 5", "PreCure 5 GoGo", "プリキュア5 鏡の国のミラクル大冒険!"] itIsNotPreCure5

itIsPreCure5 :: String -> Spec
itIsPreCure5 s =
  it ("is to be " ++ s) $ s `shouldSatisfy` isPreCure5

itIsNotPreCure5 :: String -> Spec
itIsNotPreCure5 s =
  it ("is not to be " ++ s) $ s `shouldSatisfy` (not . isPreCure5)
