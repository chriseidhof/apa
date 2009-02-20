module Test where

import qualified Data.Set as S

import WhileLanguage
import WhileProgram
import DataFlowAnalyser
import Analysis
import TableOutput

import MonotoneFramework
import Chaotic

prog :: StmtM
prog = begin
       ["r" =: AVal 1,
        "a" =: var "r" *! var "r",
        while (var "y" >! AVal 1) [ 
               "r" =: var "r" *! var "x",
               "t" =: var "y",
               "y" =: var "t" -! AVal 1
               ],
        skip
       ]

--eqs = equations ((stronglivevariables S.empty) (labelProgram prog))
--seed = seedEqs ((stronglivevariables S.empty) (labelProgram prog))

showSLV iota = resultToTable ("SLV",True,"exit","entry") .
               scan_analyze (stronglivevariables iota) . labelProgram

showLV       = resultToTable ("SLV",True,"exit","entry") .
               scan_analyze livevariables . labelProgram


