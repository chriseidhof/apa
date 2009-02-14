module MonotoneFramework where

import Types

f :: Equations -> ([L], [L]) -> ([L], [L])
f (en, ex) x = (map ($x) en, map ($x) ex)

fixpoint f x = let step = f x in
               if step == x then x else fixpoint f step

flowR :: Stmt -> FlowGraph
flowR s = [(l, l') | (l', l) <- flow s]

flow :: Stmt -> FlowGraph
flow (Ass _ _ _)       = []
flow (Skip _)          = []
flow (Seq s1 s2)       = unionL [flow s1, flow s2, [(l, init s2) | l <- final s1]]
flow (While cond l s)  = unionL [flow s, [(l, init s)], [(l', l) | l' <- final s]]

final :: Stmt -> [Label]
final (Ass _ _ l)       = [l]
final (Skip l)          = [l]
final (Seq s1 s2)       = final s2
final (While cond l s)  = [l]

init :: Stmt -> Label
init (Ass _ _ l)       = l
init (Skip l)          = l
init (Seq s1 s2)       = init s1
init (While cond l s)  = l

unionL = foldr1 union
