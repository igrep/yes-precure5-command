module ACME.Yes.PreCure5.Profiles
  ( PreCure5(..)
  , allPrecures
  , introducesHerselfAs
  , transformationPhraseOf
  , metamorphoseOf
  ) where

data PreCure5 =
  CureDream | CureRouge | CureLemonade | CureMint | CureAqua
  deriving (Show, Bounded, Enum)

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
  ++ "華麗に羽ばたく5つの心！\n"
  ++ "Yes！プリキュア5！\n"

metamorphoseOf :: [PreCure5] -> String
metamorphoseOf = transformationPhraseOf

