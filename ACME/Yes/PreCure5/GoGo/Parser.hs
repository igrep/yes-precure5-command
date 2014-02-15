module ACME.Yes.PreCure5.GoGo.Parser
  ( isPreCure5GoGo
  ) where

import Text.Parsec
import Text.Parsec.String
import Data.Char

import ACME.Yes.PreCure5.Parser (precure5)

isPreCure5GoGo :: String -> Bool
isPreCure5GoGo = isRight . parse (precure5gogo >> eof) "The argument"

precure5gogo :: Parser String
precure5gogo = do
  p5 <- precure5
  spaces
  gogo <- stringCI "GoGo"
  spaces
  exclamation <- string "!" <|> string "！" <|> return ""
  spaces
  return $ p5 ++ gogo ++ exclamation

charCI :: Char -> Parser Char
charCI c | isAlpha c = (char (toUpper c)) <|> (char (toLower c))
         | otherwise = char c

stringCI :: String -> Parser String
stringCI = mapM charCI

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False
