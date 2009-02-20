module Analysis where

import Aux

import WhileLanguage
import SemiLattice
import DataFlowAnalyser
import MonotoneFramework
import Chaotic

analyze :: (Eq lat, SemiLattice lat) => DataFlowAnalyser lat -> Program -> IterationResult lat
analyze analysis = chaotic_solving. (seedEqs `split` equations) .analysis 

