module Chaotic where

import Prelude hiding (init)
import Data.Set ((\\))
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map as M
import Program
import Types
import MonotoneFramework

prog :: StmtM
prog = begin
       ["r" =: AVal 1,
        "a" =: Var "r" *! Var "r",
        while (Var "y" >! AVal 1) [ 
               "r" =: Var "r" *! Var "x",
               "y" =: Var "y" -! AVal 1
               ],
        skip
       ]

f :: Equations -> IterationResult -> IterationResult
f (en, ex) x = (appl en, appl ex)
 where appl = M.map ($ x)

equations :: Program -> Equations
equations p = (M.fromAscList entries, M.fromAscList exits)
 where entries = map (\l -> (l, slvEntry l p)) (labels p)
       exits   = map (\l -> (l, slvExit  l p)) (labels p)

slvExit' :: Label -> IterationResult -> L
slvExit' l (_,vals)  = fromJust $ M.lookup l vals

slvEntry' :: Label -> IterationResult -> L
slvEntry' l (vals,_) = fromJust $ M.lookup l vals

slvExit :: Label -> Program -> IterationResult -> L
slvExit label p r | label `elem` final p = slvExit' label r
slvExit label p r | otherwise            = S.unions [slvEntry' l' r | (l', l) <- flowR p]

slvEntry :: Label -> Program -> IterationResult -> L
slvEntry l p r = (lv \\ killSlv bL) `S.union` genSlv bL r
 where bL = fromJust $ block l p
       lv = slvExit' l r

killSlv (Ass x _ _) = S.singleton x
killSlv _           = S.empty

genSlv (Ass x a l) r       | x `S.member` (slvExit' l r) = freeVariables a 
                           | otherwise       = S.empty
-- genSlv (MultAss asgs _) r  = S.unions [freeVariables a | (_,a)<- lastToAss asgs]
--               where lastToAss = foldr (\(x,a) acc -> if x `elem` (fst acc) then acc else ((x,a):acc)) []
genSlv x           r = freeVariables x


analyze :: Stmt -> [(Label, L)]
analyze s = error "TODO"

-- TESTING
p = labelProgram prog
i = (x, x) where x = M.fromAscList $ map (\l -> (l, iterationStart p l)) (labels p)
step = f (equations p)
test r = do let l = labels p
            mapM_ (\lbl -> putStrLn $ (show (slvEntry' lbl r, slvExit' lbl r))) l

iterationStart :: Program -> Label -> L
iterationStart p l | l `elem` final p = S.singleton "a"
                   | otherwise        = S.empty
