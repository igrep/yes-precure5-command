module ACME.Yes.PreCure5.Parser.Common
  ( charCI
  , stringCI
  , isFullyConsumedBy
  ) where

import Text.Parsec
import Text.Parsec.String
import Data.Char

isFullyConsumedBy :: Parser String -> String -> Bool
isFullyConsumedBy p = isRight . parse (p >> eof) "The argument"

charCI :: Char -> Parser Char
charCI c | isAlpha c = (char (toUpper c)) <|> (char (toLower c))
         | otherwise = char c

stringCI :: String -> Parser String
stringCI = mapM charCI

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False
