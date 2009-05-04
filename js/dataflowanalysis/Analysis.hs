module DataFlowAnalysis.Analysis where

import DataFlowAnalysis.Aux

import DataFlowAnalysis.SemiLattice
import DataFlowAnalysis.DataFlowAnalyser
import DataFlowAnalysis.MonotoneFramework
import DataFlowAnalysis.Chaotic
import DataFlowAnalysis.Program

analyze :: (Show lat, Program prg, Eq lat, SemiLattice lat) => DataFlowAnalyser prg lat -> prg -> IterationResult lat
analyze analysis = chaotic_solving. (seedEqs `split` equations) .analysis 

scan_analyze :: (Show lat, Program prg, Eq lat, SemiLattice lat) => DataFlowAnalyser prg lat -> prg -> [IterationResult lat]
scan_analyze analysis = scan_chaotic_solving. (seedEqs `split` equations) .analysis 

