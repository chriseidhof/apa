module Test where

import qualified Data.Set as S

import WhileLanguage
import WhileProgram
import WhileTypes
import DataFlowAnalyser
import Analysis
import TableOutput

import WhileFlow
import MonotoneFramework

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

progprt :: String -> StmtM
progprt v = begin
            ["r" =: AVal 1,
             "a" =: var "r" *! var "r",
             while (var "y" >! AVal 1) [ 
                    "r" =: var "r" *! var "x",
                    "t" =: var "y",
                    "y" =: var "t" -! AVal 1
                    ],
             WhileProgram.print (var v)
            ]

progmass :: String -> StmtM
progmass v = begin
                ["r" =: AVal 1,
                 "a" =: var "r" *! var "r",
                 while (var "y" >! AVal 1) [ 
                        ["y","r","y"] ==: [var "a" +! AVal 1,var "r" *! var "x", var "y" -! AVal 1]
                        ],
                 WhileProgram.print (var v)
                ]

progbc :: String -> StmtM
progbc v = begin
                ["r" =: AVal 1,
                 "a" =: var "r" *! var "r",
                 while (var "y" >! AVal 1) [ 
                        ifte (var "y" >! AVal 10) 
                         (--then
                            [WhileProgram.break,
                             "y" =: var "a" *! var "a"
                            ]
                         ,--else
                            [ ["r","y"] ==: [var "r" *! var "x", var "y" -! AVal 1],
                              continue,
                              "y" =: var "a" *! var "a"
                           ])
                        ],
                 WhileProgram.print (var v)
                ]

progbc2 :: StmtM
progbc2 = begin
                ["y" =: var "x" *! var "x",
                 while (var "x" >! AVal 0) [
                        "x" =: var "x" -! AVal 1,
                        ifte (var "y" >! AVal 1024) 
                         (--then
                            [WhileProgram.break]
                         ,--else
                            [skip]
                         ),
                         "y" =: var "x" *! var "x",
                         continue,
                         "x" =: AVal 1
                        ],
                 skip
                ]

--eqs = equations ((stronglivevariables S.empty) (labelProgram prog))
--seed = seedEqs ((stronglivevariables S.empty) (labelProgram prog))

showSLV iota = resultToTable ("SLV",True,"exit","entry") .
               scan_analyze (stronglivevariables iota) . labelProgram

showLV       = resultToTable ("LV",True,"exit","entry") .
               scan_analyze livevariables . labelProgram


showAE = resultToTable ("AE",False,"entry","exit") .
         scan_analyze availableexpressions . labelProgram
