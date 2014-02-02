module ACME.Yes.PreCure5
  ( PreCure5(..)
  , introducesHerselfAs
  , transformationPhraseOf
  , isPreCure5
  , chooseTransformationPhrase
  ) where

import System.Random
import Text.Parsec hiding (State)
import Text.Parsec.String
import Data.Char
import Control.Monad.State

data PreCure5 =
  CureDream | CureRouge | CureLemonade | CureMint | CureAqua
  deriving (Show, Bounded, Enum)

instance Random PreCure5 where
  randomR (a, b) g =
    let (p, g') = randomR (fromEnum a, fromEnum b) g
        in (toEnum p, g')
  random = randomR (minBound, maxBound)

allPrecures :: [PreCure5]
allPrecures = [minBound..maxBound]

introducesHerselfAs :: PreCure5 -> String
introducesHerselfAs CureDream    = "大いなる希望の力、キュアドリーム！"
introducesHerselfAs CureRouge    = "情熱の赤い炎、キュアルージュ！"
introducesHerselfAs CureLemonade = "はじけるレモンの香り、キュアレモネード！"
introducesHerselfAs CureMint     = "安らぎの緑の大地、キュアミント！"
introducesHerselfAs CureAqua     = "知性の青き泉、キュアアクア！"

transformationPhraseOf :: [PreCure5] -> String
transformationPhraseOf ps =
     "プリキュア！メタモルフォーゼ！\n"
  ++ (unlines $ map introducesHerselfAs ps)
  ++ "希望の力と未来の光！\n"
  ++ "華麗に羽ばたく5つの心！"
  ++ "Yes！プリキュア5！"

isPreCure5 :: String -> Bool
isPreCure5 = isRight . parse precure5 "The argument"

chooseTransformationPhrase :: RandomGen g => g -> (String, g)
chooseTransformationPhrase g =
  (flip runState) g $ do
    i <- randomRSt (1, 20)
    p <- randomSt
    return $ transformationPhraseOf $ fiveOrSinglePreCure i p

fiveOrSinglePreCure :: Int -> PreCure5 -> [PreCure5]
fiveOrSinglePreCure 5 _ = allPrecures
fiveOrSinglePreCure _ p = [p]

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

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt r = state $ randomR r

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random