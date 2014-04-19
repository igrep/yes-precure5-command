module ACME.Yes.PreCure5.GoGo.Parser
  ( isPreCure5GoGo
  ) where

import Text.Parsec
import Text.Parsec.String

import ACME.Yes.PreCure5.Parser (precure5)
import ACME.Yes.PreCure5.Parser.Common

isPreCure5GoGo :: String -> Bool
isPreCure5GoGo = isFullyConsumedBy precure5gogo

precure5gogo :: Parser String
precure5gogo = do
  p5 <- precure5
  spaces
  gogo <- stringCI "GoGo"
  spaces
  exclamation <- string "!" <|> string "！" <|> return ""
  spaces
  return $ p5 ++ gogo ++ exclamation
