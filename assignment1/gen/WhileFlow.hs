module WhileFlow where
import Prelude hiding (init)

import WhileTypes
import WhileLanguage


init :: Stmt -> Label
init (Ass _ _ l)       = l
init (MultAss _ l)     = l
init (Print _ l)       = l
init (Skip l)          = l
init (Continue l)      = l
init (Break l)         = l
init (Seq s1 s2)       = init s1
init (If cond l _ _)   = l
init (While cond l _)  = l

final :: Stmt -> [Label]
final (Ass _ _ l)       = [l]
final (MultAss _ l)     = [l]
final (Print _ l)       = [l]
final (Skip l)          = [l]
final (Continue _)      = [ ]
final (Break _)         = [ ]
final (Seq s1 s2)       = final s2
final (If _ _ s1 s2)    = final s1 ++ final s2
final (While cond l s)  = [l] ++ breaksOf s


flow :: Stmt -> FlowGraph
flow (Ass _ _ _)       = [ ]
flow (MultAss _ _)     = [ ]
flow (Print _ _)       = [ ]
flow (Skip _)          = [ ]
flow (Continue _)      = [ ]
flow (Break _)         = [ ]
flow (Seq s1 s2)       = concat [ flow s1, flow s2, [(l, init s2) | l <- final s1] ]
flow (If c l s1 s2)    = concat [ flow s1, flow s2, [(l, init s1),(l,init s2)]     ]
flow (While cond l s)  = concat [ flow s, [(l, init s)], [(l', l) | l' <- final s], [(l',l) | l' <- continuesOf s] ]

flowR :: Stmt -> FlowGraph
flowR s = [(l, l') | (l', l) <- flow s]


continuesOf :: Stmt -> [Label]
continuesOf (Ass _ _ _)       = [ ]
continuesOf (MultAss _ _)     = [ ]
continuesOf (Print _ _)       = [ ]
continuesOf (Skip _)          = [ ]
continuesOf (Continue l)      = [l]
continuesOf (Break _)         = [ ]
continuesOf (Seq s1 s2)       = continuesOf s1 ++ continuesOf s2 
continuesOf (If c _ s1 s2)    = continuesOf s1 ++ continuesOf s2
continuesOf (While cond l s)  = [ ]

breaksOf :: Stmt -> [Label]
breaksOf (Ass _ _ _)       = [ ]
breaksOf (MultAss _ _)     = [ ]
breaksOf (Print _ _)       = [ ]
breaksOf (Skip _)          = [ ]
breaksOf (Continue _)      = [ ]
breaksOf (Break l)         = [l]
breaksOf (Seq s1 s2)       = breaksOf s1 ++ breaksOf s2 
breaksOf (If c _ s1 s2)    = breaksOf s1 ++ breaksOf s2
breaksOf (While cond l s)  = [ ]


