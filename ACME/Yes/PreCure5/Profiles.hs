module ACME.Yes.PreCure5.Profiles
  ( PreCure5(..)
  , allPrecures
  , introducesHerselfAs
  , transformationPhraseOf
  , metamorphoseOf
  ) where

import ACME.Yes.PreCure5.Class

import qualified Data.Set as S

data PreCure5 =
  CureDream | CureRouge | CureLemonade | CureMint | CureAqua
  deriving (Show, Bounded, Enum, Eq, Ord)

instance PreCure PreCure5 where
  allPrecures = S.fromAscList [minBound..maxBound]

  introducesHerselfAs CureDream    = "大いなる希望の力、キュアドリーム！"
  introducesHerselfAs CureRouge    = "情熱の赤い炎、キュアルージュ！"
  introducesHerselfAs CureLemonade = "はじけるレモンの香り、キュアレモネード！"
  introducesHerselfAs CureMint     = "安らぎの緑の大地、キュアミント！"
  introducesHerselfAs CureAqua     = "知性の青き泉、キュアアクア！"

  transformationPhraseOf ps =
       "プリキュア！メタモルフォーゼ！\n"
    ++ (unlines $ map introducesHerselfAs $ S.toAscList ps)
    ++ "希望の力と未来の光！\n"
    ++ "華麗に羽ばたく5つの心！\n"
    ++ "Yes！プリキュア5！\n"

metamorphoseOf :: S.Set PreCure5 -> String
metamorphoseOf = transformationPhraseOf
