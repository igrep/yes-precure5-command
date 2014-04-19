module ACME.Yes.PreCure5.Class
  ( PreCure
  , introducesHerselfAs
  , transformationPhraseOf
  , allPrecures
  ) where

import Data.Set (Set)

class PreCure p where
  introducesHerselfAs :: p -> String
  transformationPhraseOf :: Set p -> String
  allPrecures :: Set p
