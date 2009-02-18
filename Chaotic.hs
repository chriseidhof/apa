module Chaotic where

import Prelude hiding (init)
import Data.Set ((\\))
import Data.Maybe (fromJust)
import Data.List (sortBy, transpose, intercalate)
import qualified Data.Set as S
import qualified Data.Map as M
import Program
import Types
import MonotoneFramework

prog1 :: StmtM
prog1 = begin
        ["r" =: AVal 1,
         "a" =: Var "r" *! Var "r",
         while (Var "y" >! AVal 1) [ 
                "r" =: Var "r" *! Var "x",
                "t" =: Var "y",
                "y" =: Var "t" -! AVal 1
                ],
         Program.print (Var "r")
        ]

       
prog2 = begin
        ["r" =: AVal 1,
         "a" =: Var "r" *! Var "r",
         while (Var "y" >! AVal 1) [ 
                "r" =: Var "r" *! Var "x",
                "t" =: Var "y",
                "y" =: Var "t" -! AVal 1
                ],
         skip
        ]


f :: Equations -> IterationResult -> IterationResult
f (en, ex) x = (appl en, appl ex)
 where appl = M.map ($ x)

equations :: Program -> Equations
equations p = (M.fromAscList entries, M.fromAscList exits)
 where entries = map (\l -> (l, slvEntry p l)) (labels p)
       exits   = map (\l -> (l, slvExit  p l)) (labels p)

slvExit' :: Label -> IterationResult -> L
slvExit' l  = fromJust . M.lookup l . snd

slvEntry' :: Label -> IterationResult -> L
slvEntry' l = fromJust . M.lookup l . fst

slvExit :: Program -> Label -> IterationResult -> L
slvExit p label r | label `elem` final p = slvExit' label r
                  | otherwise            = S.unions [slvEntry' l' r | (l', l) <- flowR p, l == label]

slvEntry :: Program -> Label -> IterationResult -> L
slvEntry p l r = (lv \\ killSlv bL) `S.union` genSlv bL lv
 where bL = fromJust $ block p l
       lv = slvExit' l r

killSlv (Ass x _ _) = S.singleton x
killSlv _           = S.empty

genSlv (Ass x a l) lv      | x `S.member` lv = freeVariables a 
                           | otherwise       = S.empty
-- genSlv (MultAss asgs _) r  = S.unions [freeVariables a | (_,a)<- lastToAss asgs]
--               where lastToAss = foldr (\(x,a) acc -> if x `elem` (fst acc) then acc else ((x,a):acc)) []
genSlv x           _ = freeVariables x


analyze :: Stmt -> L -> IterationResult
analyze s iota = let eqs =  equations s
                 in fixpoint (f eqs) (startIteration s iota)

startIteration p iota = let vals = map (\l->((l,S.empty),(l,if(l`elem`final p) then iota else S.empty))) (labels p)
                            (env,exv) = unzip vals
                        in  (M.fromAscList env, M.fromAscList exv)

data Cell = Cell {name :: String, lattice :: L}
data Row  = Row String [L]
  deriving Show

formatTable :: [Row] -> String
formatTable = intercalate " \\\\ \n" . map showRow

showRow (Row s l) = s ++ " & " ++ lattices (map showLattice l)
   where lattices = intercalate "&" . map latexSet
         latexSet "" = "\\emptyset"
         latexSet x  = "\\{" ++ x ++ "\\}"

showLattice = intercalate "," . S.toList

table p iota = let first   = startIteration p iota
                   eqs     = equations p
                   allIterations = takeWhileChanging (iterate (f eqs) first)
                   toRow cells = if allEqual (map name cells)
                                 then Row (name (head cells)) (map lattice cells)
                                 else error "not all cells have been grouped correctly"
               in map toRow $ transpose $ map formatIteration allIterations -- zip before after

-- tableMax = 10

formatIteration :: IterationResult -> [Cell]
formatIteration (entries, exits) = concat $ zipWith two (format "entry" entries) (format "exit" exits)
 where sortMap    = sortBy (\x y -> compare (fst x) (fst y)) . M.toAscList
       format s   = map (mkCell s) . sortMap
       mkCell s (l,r) = Cell ("SLV_{" ++ s ++ "} & \\hspace{-10pt} (" ++ show l ++ ")") r
       two a b    = [a, b]

takeWhileChanging []                   = []
takeWhileChanging (x:y:xs) | x == y    = [x,y]
takeWhileChanging (x:y:ys) | otherwise = x:(takeWhileChanging (y:ys))

allEqual []     = True
allEqual (x:xs) = all (== x) xs

section2 iota = do putStrLn "$\\begin{array}{llccccccccccccccccccccc}"
                   putStrLn $ formatTable . table p $ S.singleton iota
                   putStrLn "\\end{array}$"

main = section2

-- TESTING
p = labelProgram prog1
step = f (equations p)
test r = do let l = labels p
            mapM_ (\lbl -> putStrLn $ (show lbl ++ ": " ++ (unwords $ S.toList $ slvEntry' lbl r) ++ " | " ++ (unwords $ S.toList $ slvExit' lbl r))) l

