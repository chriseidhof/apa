module Chaotic where

import qualified Data.Map as M

import MonotoneFrameworks

chaotic_solving :: Equations lat -> IterationResult lat
chaotic_solving = fixpoint chaoticstep boooooottooomm

fixpoint f x = let x' = f x in
               if  x' == x then x else fixpoint f x'

chaoticstep :: Equations lat -> IterationResult lat -> IterationResult lat
chaoticstep (en, ex) x = (appl en, appl ex)
 where appl = M.map ($ x)

startIteration p iota = let vals = map (\l->((l,S.empty),(l,if(l`elem`final p) then iota else S.empty))) (labels p)
                            (env,exv) = unzip vals
                        in  (M.fromAscList env, M.fromAscList exv)

