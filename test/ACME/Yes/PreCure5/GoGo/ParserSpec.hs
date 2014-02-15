module ACME.Yes.PreCure5.GoGo.ParserSpec where

import ACME.Yes.PreCure5.GoGo.Parser
import Test.Hspec
import Control.Monad

spec :: Spec
spec = do
  describe "isPreCure5GoGo" $ do
    it "is not to be blank" $
      "" `shouldSatisfy` (not . isPreCure5GoGo)

    forM_ wordsToBePreCure5GoGo $ \s ->
      it ("is to be " ++ s) $ s `shouldSatisfy` isPreCure5GoGo

wordsToBePreCure5GoGo :: [String]
wordsToBePreCure5GoGo = do
  precure <- ["プリキュア", "PreCure", "Precure", "precure", "PRECURE"]
  five <- ["5", "５"]
  gogo <- ["GoGo", "gogo", "GOGO"]
  exclamation <- ["", "!", "！"]
  space <- [" ", ""]
  return $ precure ++ space ++ five ++ space ++ gogo ++ exclamation
