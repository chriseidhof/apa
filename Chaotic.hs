module Chaotic where

import Prelude hiding (init)
import Control.Monad
import Data.Set ((\\))
import Data.Maybe (fromJust)
import qualified Data.Set as S
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

f :: Equations -> ([L], [L]) -> ([L], [L])
f (en, ex) x = (appl en, appl ex)
 where appl = map ($x)

block :: Label -> Program -> Maybe Stmt
block l b@(Ass _ _ l')    | l == l'   = Just b
block l b@(Skip l')       | l == l'   = Just b
block l b@(Seq s1 s2)                 = block l s1 `mplus` block l s2
block l b@(While _ l' s)  | l == l'   = Just b
                          | otherwise = block l s
block _ _                             = Nothing

labels :: Program -> [Label]
labels (Ass _ _ l)   = [l]
labels (Skip l)      = [l]
labels (Seq s1 s2)   = labels s1 ++ labels s2
labels (While _ l s) = [l] ++ labels s

equations :: Program -> Equations
equations p = (entries, exits)
 where entries = map (\l -> slvEntry l p) (labels p)
       exits   = map (\l -> slvExit  l p) (labels p)
-- type Equation = ([L], [L]) -> L -- (Entries, Exits)

iota = S.empty

type Program = Stmt
type IterationResult = ([L], [L])

-- This looks in the previous definition
slvExit' :: Label -> IterationResult -> L
slvExit' l (_,vals) = vals !! (l - 1) -- TODO: this is ugly

slvEntry' :: Label -> IterationResult -> L
slvEntry' l (vals,_) = vals !! (l - 1) -- TODO: this is ugly

slvExit :: Label -> Program -> IterationResult -> L
slvExit label p r | label `elem` final p = slvExit' label r
slvExit label p r | otherwise            = S.unions [slvEntry' l' r | (l', l) <- flowR p]

slvEntry :: Label -> Program -> IterationResult -> L
slvEntry l p r = (lv \\ killSlv bL) `S.union` genSlv bL r
 where bL = fromJust $ block l p
       lv = slvExit' l r

killSlv (Ass x _ _) = S.singleton x
killSlv _           = S.empty

genSlv (Ass x a l) r | x `S.member` (slvExit' l r) = freeVariables a 
                     | otherwise                   = S.empty
genSlv x           r = freeVariables x

analyze :: Stmt -> [(Label, L)]
analyze s = error "TODO"

-- TESTING
p = labelProgram prog
i = (x, x) where x = map (iterationStart p) (labels p)
step = f (equations p)
test r = do let l = labels p
            mapM_ (\lbl -> putStrLn $ (show (slvEntry' lbl r, slvExit' lbl r))) l

iterationStart :: Program -> Label -> L
iterationStart p l | l `elem` final p = S.singleton "a"
                   | otherwise        = S.empty
