module DataFlowAnalyser where

import Prelude hiding (init)
import qualified Data.Set as S 
import Data.Set ((\\))
import qualified Data.Map as M
import Maybe
import Aux

import SemiLattice
import MonotoneFramework
import Program

type DataFlowAnalyser prg lat = prg -> MonotoneFramework lat
createDataFlowAnalyser :: (Program prg)=> FlowInfoGen prg -> MeasureGen prg lat -> DataFlowAnalyser prg lat
createDataFlowAnalyser fg mg p = let flowinfo@(vert,_,_) = fg p
                                     (iota,trfOfLabel)   = mg p
                                    -- trfOflabel      = (trfOfstm.fromJust.block p)
                                     trfunctions         = M.fromAscList $ map (id `split` trfOfLabel) vert
                                 in MF flowinfo (iota, trfunctions)
       
type FlowInfoGen prg = prg -> ([Label],FlowGraph,[Label])
createFlowInfoGen :: (Program prg) => (prg->[Label], prg -> FlowGraph, prg->[Label]) -> FlowInfoGen prg
createFlowInfoGen (verticesOf,edgesOf,extremesOf) = \p -> (verticesOf p, edgesOf p, extremesOf p)

type MeasureGen prg lat = prg -> (lat, Label -> (lat->lat))
createMeasureGen :: ((prg -> lat), TransferFunctionsLabGen prg lat) -> MeasureGen prg lat
createMeasureGen (iotaOf, transfOf) = \p -> (iotaOf p, transfOf p)


type TransferFunctionsStmGen prg lat =  prg -> (prg-> (lat->lat)) 
type TransferFunctionsLabGen prg lat =  prg -> (Label-> (lat->lat)) 


-- backward and forward flows
backward,forward :: (Program prg) => FlowInfoGen prg
backward = createFlowInfoGen (labels, flowR, final)
forward  = createFlowInfoGen (labels, flow , singl.init)


--Set Lattices

--union
--may :: (Program prg, Ord a) => (prg -> S.Set a,prg -> (prg -> S.Set a -> S.Set a)) -> MeasureGen prg (SetUL a)
may :: (Program prg, Ord a) => (prg -> S.Set a,prg -> (Label -> S.Set a -> S.Set a)) -> MeasureGen prg (SetUL a)
may (ioter,tf_sets)  = usplit (fromSet.ioter , \p stmt -> fromSet.(tf_sets p stmt).toSet)

--intersection
--must :: (Program prg, Ord a) => (prg -> S.Set a,prg -> (prg -> S.Set a -> S.Set a)) -> MeasureGen prg (SetIL a)
must :: (Program prg, Ord a) => (prg -> S.Set a,prg -> (Label -> S.Set a -> S.Set a)) -> MeasureGen prg (SetIL a)
must (ioter,tf_sets)  = usplit (fromSet.ioter , \p stmt -> topProtect (fromSet.(tf_sets p stmt).toSet) )
   --UNSAFE toSet application

topProtect f Top = Top
topProtect f x   = f x

-- when our lattice is made of sets, gen and kill functions are a common pattern

genkill :: (Ord a) => ((prg -> S.Set a),(prg -> S.Set a)) -> (prg -> S.Set a -> S.Set a)
genkill (gen,kill) = depgenkill (const.gen,const.kill)

depgenkill :: (Ord a) => ((prg -> S.Set a -> S.Set a),(prg -> S.Set a -> S.Set a)) -> (prg -> S.Set a -> S.Set a)
depgenkill (gen,kill) = \bl x -> (x \\ kill bl x) `S.union` gen bl x





