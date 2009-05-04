module DataFlowAnalysis.Chaotic where

import qualified Data.Set as S
import qualified Data.Map as M

import DataFlowAnalysis.MonotoneFramework

chaotic_solving :: (Eq lat) => (IterationResult lat, Equations lat) -> IterationResult lat
chaotic_solving (st,eqs) = fixpoint (chaoticstep eqs) st

fixpoint f x = let x' = f x in
               if  x' == x then x else fixpoint f x'

chaoticstep :: Equations lat -> IterationResult lat -> IterationResult lat
chaoticstep (op, cl) x = (appl op, appl cl)
 where appl = M.map ($ x)

scan_chaotic_solving :: (Eq lat) => (IterationResult lat,Equations lat) -> [IterationResult lat]
scan_chaotic_solving (st,eqs) = takeWhileChanging (iterate (chaoticstep eqs) st)

takeWhileChanging [] = []
takeWhileChanging (x:y:xs) | x == y = [x,y]
takeWhileChanging (x:y:ys) | otherwise = x:(takeWhileChanging (y:ys))



