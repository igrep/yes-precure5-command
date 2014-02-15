module ACME.Yes.PreCure5.Class
  ( PreCure
  , introducesHerselfAs
  , transformationPhraseOf
  , allPrecures
  ) where

class PreCure p where
  introducesHerselfAs :: p -> String
  transformationPhraseOf :: [p] -> String
  allPrecures :: [p]
