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

scan_chaotic_solving :: (Eq lat) => (IterationResult lat,Equations lat) -> [IterationResult lat]
scan_chaotic_solving (st,eqs) = takeWhileChanging (iterate (f eqs) first)


table p iota = let first   = startIteration p iota
                   eqs     = equations p
                   allIterations = takeWhileChanging (iterate (f eqs) first)
                   toRow cells = if allEqual (map name cells)
                                 then Row (name (head cells)) (map lattice cells)
                                 else error "not all cells have been grouped correctly"
               in map toRow $ transpose $ map formatIteration allIterations -- zip before after

--formatIteration :: IterationResult -> [Cell]
--formatIteration (entries, exits) = concat $ zipWith two (format "entry" entries) (format "exit" exits)
-- where sortMap    = sortBy (\x y -> compare (fst x) (fst y)) . M.toAscList
--       format s   = map (mkCell s) . sortMap
--       mkCell s (l,r) = Cell ("SLV_{" ++ s ++ "}(" ++ show l ++ ")") r
--       two a b    = [a, b]
--
takeWhileChanging []                   = []
takeWhileChanging (x:y:xs) | x == y    = [x,y]
takeWhileChanging (x:y:ys) | otherwise = x:(takeWhileChanging (y:ys))

allEqual []     = True
allEqual (x:xs) = all (== x) xs

{--
data Cell = Cell {name :: String, lattice :: L}
data Row  = Row String [L]

formatTable :: [Row] -> String
formatTable = intercalate " \\\\ \n" . map showRow

showRow (Row s l) = s ++ " & " ++ lattices (map showLattice l)
   where lattices = intercalate "&" . map latexSet
         latexSet "" = "\\emptyset"
         latexSet x  = "\\{" ++ x ++ "\\}"

showLattice = intercalate "," . S.toList


-- TESTING
p = labelProgram prog
step = f (equations p)
test r = do let l = labels p
            mapM_ (\lbl -> putStrLn $ (show lbl ++ ": " ++ (unwords $ S.toList $ slvEntry' lbl r) ++ " | " ++ (unwords $ S.toList $ slvExit' lbl r))) l

test2 = putStrLn $ formatTable . table p $ S.singleton "r"
--}
