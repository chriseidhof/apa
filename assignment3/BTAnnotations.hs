module BTAnnotations where
import SemiLattice
import Constraints

data Annot = S | D
instance SemiLattice Annot where
  bottom = S
  S \/ S = S
  _ \/ _ = D

