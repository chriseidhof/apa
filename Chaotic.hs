module Chaotic where

import Prelude hiding (init)
import Data.List (union)
import Program
import Types

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

ref :: Label -> Stmt -> Stmt
ref =  undefined
equations :: FlowGraph -> Stmt -> Equations
equations = undefined

analyze :: Stmt -> [(Label, L)]
analyze = error "TODO"
