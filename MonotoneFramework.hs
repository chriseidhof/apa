module MonotoneFramework where

import Types
import Prelude hiding (init)
import Data.List (union)
import Control.Monad

fixpoint f x = let step = f x in
               if step == x then x else fixpoint f step

flowR :: Stmt -> FlowGraph
flowR s = [(l, l') | (l', l) <- flow s]

flow :: Stmt -> FlowGraph
flow (Ass _ _ _)       = []
flow (MultAss _ _)     = []
flow (Print _ _)       = []
flow (Skip _)          = []
--flow (Continue _)
--flow (Break _)
flow (Seq s1 s2)       = unionL [flow s1, flow s2, [(l, init s2) | l <- final s1]]
flow (While cond l s)  = unionL [flow s, [(l, init s)], [(l', l) | l' <- final s]]

final :: Stmt -> [Label]
final (Ass _ _ l)       = [l]
final (MultAss _ l)     = [l]
final (Print _ l)       = [l]
final (Skip l)          = [l]
--final (Continue _)
--final (Break _)
final (Seq s1 s2)       = final s2
final (While cond l s)  = [l]

init :: Stmt -> Label
init (Ass _ _ l)       = l
init (MultAss _ l)     = l
init (Print _ l)       = l
init (Skip l)          = l
--init (Continue _)
--init (Break _)
init (Seq s1 s2)       = init s1
init (While cond l s)  = l

unionL = foldr1 union

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
