module Chaotic where

import qualified Data.Set as S
import qualified Data.Map as M

import MonotoneFramework

chaotic_solving :: (Eq lat) => (IterationResult lat, Equations lat) -> IterationResult lat
chaotic_solving (st,eqs) = fixpoint (chaoticstep eqs) st

fixpoint f x = let x' = f x in
               if  x' == x then x else fixpoint f x'

chaoticstep :: Equations lat -> IterationResult lat -> IterationResult lat
chaoticstep (en, ex) x = (appl en, appl ex)
 where appl = M.map ($ x)



