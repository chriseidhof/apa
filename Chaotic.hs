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
block l b@(MultAss _ l')  | l == l'   = Just b
block l b@(Print _ l')    | l == l'   = Just b
block l b@(Skip l')       | l == l'   = Just b
block l b@(Continue l')   | l == l'   = Just b
block l b@(Break l')      | l == l'   = Just b
block l b@(Seq s1 s2)                 = block l s1 `mplus` block l s2
block l b@(While _ l' s)  | l == l'   = Just b
                          | otherwise = block l s
block _ _                             = Nothing

labels :: Program -> [Label]
labels (Ass _ _ l)   = [l]
labels (MultAss _ l) = [l]
labels (Print _ l)   = [l]
labels (Skip l)      = [l]
labels (Continue l)  = [l]
labels (Break l)     = [l]
labels (Seq s1 s2)   = labels s1 ++ labels s2
labels (While _ l s) = [l] ++ labels s

equations :: FlowGraph -> Stmt -> Equations
equations graph s = undefined --slvExit
-- type Equation = ([L], [L]) -> L -- (Entries, Exits)

iota = S.empty

type Program = Stmt

slvExit :: Label -> Program -> L -> L
slvExit label p lv | label `elem` final p = lv
slvExit label p lv | otherwise            = S.unions [slvEntry l' p lv | (l', l) <- flowR p]

slvEntry :: Label -> Program -> L -> L
slvEntry l p lV = (slvExit l p lV \\ killSlv bL) `S.union` genSlv bL lV
 where bL = fromJust $ block l p

killSlv (Ass x _ _) = S.singleton x
killSlv _           = S.empty

genSlv (Ass x a _) lv | x `S.member` lv = freeVariables a 
                      | otherwise       = S.empty
genSlv (MultAss asgs _) lv  = S.unions [freeVarialbes a | (_,a)<- lastToAss asgs]
              where lastToAss = foldr (\(x,a) acc -> if x `elem` (fst acc) then acc else ((x,a):acc)) []
genSlv x           lv = freeVariables x


analyze :: Stmt -> [(Label, L)]
analyze s = error "TODO"
