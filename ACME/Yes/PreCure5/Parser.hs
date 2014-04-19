module ACME.Yes.PreCure5.Parser
  ( isPreCure5
  , precure5
  ) where

import Text.Parsec
import Text.Parsec.String

import ACME.Yes.PreCure5.Parser.Common

isPreCure5 :: String -> Bool
isPreCure5 = isFullyConsumedBy precure5

precure5 :: Parser String
precure5 = do
  precure <- (string "プリキュア" <|> stringCI "PreCure")
  spaces
  five <- (string "5" <|> string "５")
  spaces
  return $ precure ++ five
