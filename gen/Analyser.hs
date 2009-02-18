module Analyser where

import Prelude hiding (init)
import qualified Data.Map as M

import WhileTypes
import WhileLanguage
import WhileFlow
import MonotoneFramework


type DataFlowAnalyser lat = Program -> MonotoneFramework lat
createDataFlowAnalyser :: FlowInfoGen -> MeasureGen lat -> DataFlowAnalyser lat
createDataFlowAnalyser fg mg = \p -> MF (fg p) (mg p)

type FlowInfoGen = Program -> ([Label],FlowGraph,[Label])
createFlowInfoGen :: (Program->[Label], Program -> FlowGraph, Program->[Label]) -> FlowInfoGen
createFlowInfoGen (verticesOf,edgesOf,extremesOf) = \p -> (verticesOf p, edgesOf p, extremesOf p)

type MeasureGen lat = Program -> (lat, M.Map Label (lat->lat))
createMeasureGen :: (Program -> lat) -> (Program -> M.Map Label (lat->lat)) -> MeasureGen
createMeasureGen (iotaOf, transfOf) = \p -> (iotaOf p, transfOf p)


backward,forward :: FlowInfoGen
backward = createFlowInfoGen (labels, flowR, final)
forward  = createFlowInfoGen (labels, flow , singl.init)


genkill :: (Stmt -> lat) -> (Stmt -> lat) -> lat -> MeasureGen
genkill gen kill = depgenkill (const.gen) (const.kill)

depgenkill :: (Stmt -> lat -> lat) -> (Stmt -> lat -> lat) -> lat -> MeasureGen
depgenkill gen kill iota = createMeasureGen (const iota) gentrfs
    where gentrfs p = M.fromAscList $ map (\lab->(lab,trf p lab)) (labels p)
          trf p lab = let bl = (block p lab)
                      in \x ->   (x \\ kill bl x) `S.union` gen bl x

singl :: a -> [a]
singl = return
