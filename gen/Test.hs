module Test where

import qualified Data.Set as S

import WhileLanguage
import WhileProgram
import DataFlowAnalyser
import Analysis


import MonotoneFramework

prog :: StmtM
prog = begin
       ["r" =: AVal 1,
        "a" =: Var "r" *! Var "r",
        while (Var "y" >! AVal 1) [ 
               "r" =: Var "r" *! Var "x",
               "t" =: Var "y",
               "y" =: Var "t" -! AVal 1
               ],
        skip
       ]

mf = ( (stronglivevariables S.empty) (labelProgram prog))
sv = seedEqs mf
eqs = equations mf

