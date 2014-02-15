module ACME.Yes.PreCure5.GoGo.Profiles
  ( PreCure5GoGo(..)
  , allPrecures
  , introducesHerselfAs
  , transformationPhraseOf
  , metamorphoseOf
  ) where

import ACME.Yes.PreCure5.Class
import ACME.Yes.PreCure5.Profiles

import qualified Data.Set as S

data PreCure5GoGo =
  PreCure5 PreCure5 | MilkyRose
  deriving (Show, Eq, Ord)

instance PreCure PreCure5GoGo where
  allPrecures = S.insert MilkyRose ( S.map PreCure5 allPrecures )

  introducesHerselfAs (PreCure5 p) = introducesHerselfAs p
  introducesHerselfAs MilkyRose    = "青いバラは秘密のしるし！ ミルキィローズ！"

  transformationPhraseOf ps = yes ++ rose
    where
      (roses, p5) = S.partition (== MilkyRose) ps
      yes  = if S.null p5    then "" else transformationPhraseOf $ S.map toPreCure5 p5
      rose = if S.null roses then "" else milyRoseTransformation
      milyRoseTransformation =
           "スカイローズ・トランスレイト！\n"
        ++ introducesHerselfAs MilkyRose ++ "\n"

toPreCure5 :: PreCure5GoGo -> PreCure5
toPreCure5 (PreCure5 p) = p
toPreCure5 MilkyRose = error "MilkyRose isn't a character of Yes! PreCure 5"
