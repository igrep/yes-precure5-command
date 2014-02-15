module ACME.Yes.PreCure5.Parser
  ( isPreCure5
  , precure5
  ) where

import Text.Parsec
import Text.Parsec.String
import Data.Char

isPreCure5 :: String -> Bool
isPreCure5 = isRight . parse precure5 "The argument"

precure5 :: Parser String
precure5 = do
  precure <- (string "プリキュア" <|> stringCI "PreCure")
  spaces
  five <- (string "5" <|> string "５")
  return $ precure ++ five

charCI :: Char -> Parser Char
charCI c | isAlpha c = (char (toUpper c)) <|> (char (toLower c))
         | otherwise = char c

stringCI :: String -> Parser String
stringCI = mapM charCI

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False
