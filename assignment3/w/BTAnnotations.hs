module BTAnnotations where

import SemiLattice
import Constraints

data Annot = S | D deriving (Eq, Show)
instance SemiLattice Annot where
  bottom = S
  S \/ S = S
  _ \/ _ = D

