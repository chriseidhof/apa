module DataFlowAnalyser where

import Prelude hiding (init)
import qualified Data.Set as S 
import Data.Set ((\\))
import qualified Data.Map as M
import Maybe
import Aux

import WhileTypes
import WhileLanguage
import WhileFlow
import MonotoneFramework
import SemiLattice

type DataFlowAnalyser lat = Program -> MonotoneFramework lat
createDataFlowAnalyser :: FlowInfoGen -> MeasureGen lat -> DataFlowAnalyser lat
createDataFlowAnalyser fg mg p = let flowinfo        = fg p
                                     (iota,trfOfstm) = mg p
                                     trfOflabel      = (trfOfstm.fromJust.block p)
                                     trfunctions     = M.fromAscList $ map (id `split` trfOflabel) (labels p)
                                 in MF flowinfo (iota, trfunctions)
       
type FlowInfoGen = Program -> ([Label],FlowGraph,[Label])
createFlowInfoGen :: (Program->[Label], Program -> FlowGraph, Program->[Label]) -> FlowInfoGen
createFlowInfoGen (verticesOf,edgesOf,extremesOf) = \p -> (verticesOf p, edgesOf p, extremesOf p)

type MeasureGen lat = Program -> (lat, Stmt -> (lat->lat))
createMeasureGen :: ((Program -> lat), TransferFunctionsStmGen lat) -> MeasureGen lat
createMeasureGen (iotaOf, transfOf) = \p -> (iotaOf p, transfOf p)

type TransferFunctionsStmGen lat =  (Program -> (Stmt-> (lat->lat)) )


-- backward and forward flows
backward,forward :: FlowInfoGen
backward = createFlowInfoGen (labels, flowR, final)
forward  = createFlowInfoGen (labels, flow , singl.init)


--Set Lattices

--union
may :: (Ord a) => (Program -> S.Set a,Program -> (Stmt -> S.Set a -> S.Set a)) -> MeasureGen (SetUL a)
may (ioter,tf_sets)  = usplit (fromSet.ioter , \p stmt -> fromSet.(tf_sets p stmt).toSet)

--intersection
must :: (Ord a) => (Program -> S.Set a,Program -> (Stmt -> S.Set a -> S.Set a)) -> MeasureGen (SetIL a)
must (ioter,tf_sets)  = usplit (fromSet.ioter , \p stmt -> fromSet.(tf_sets p stmt).toSet)
   --UNSAFE toSet application

-- when our lattice is made of sets, gen and kill functions are a common pattern

genkill :: (Ord a) => ((Stmt -> S.Set a),(Stmt -> S.Set a)) -> (Stmt -> S.Set a -> S.Set a)
genkill (gen,kill) = depgenkill (const.gen,const.kill)

depgenkill :: (Ord a) => ((Stmt -> S.Set a -> S.Set a),(Stmt -> S.Set a -> S.Set a)) -> (Stmt -> S.Set a -> S.Set a)
depgenkill (gen,kill) = \bl x -> (x \\ kill bl x) `S.union` gen bl x




--some common analysis

availableexpressions = createDataFlowAnalyser forward  (must (iotaAE,\p->genkill (genAE,killAE p)) ) 
iotaAE   = const S.empty
killAE = killVB
gen (Ass x a _) = S.filter (not.(x`S.member`).freeVariables) (freeArithmeticalExpressions a)
genAE blk = freeArithmeticalExpressions blk

reachingdefinitions    = createDataFlowAnalyser forward  (may  (iotaRD,\p->genkill(genRD,killRD p) ))
iotaRD p = S.map (\x->(x,Nothing)) (freeVariables p)
genRD (Ass x a l)         = S.singleton (x,Just l)
genRD (MultAss assg l)    = S.fromList  [(x,Just l)|(x,_)<-assg]
genRD _                   = S.empty
killRD p (Ass x a l)      = S.singleton (x,Nothing) `S.union`
                            S.fromList [(x,Just l')|l'<-labels p, isAssgTo x (block p l')]
     where isAssgTo x (Just (Ass v _ _))      = v==x
           isAssgTo x (Just (MultAss asgs _)) = x `elem` map fst asgs
           isAssgTo x _ = False
killRD p (MultAss asgs l) = S.unions [killRD p (Ass x a l) | (x,a)<-asgs]
killRD p _                = S.empty

verybusyexpressions    = createDataFlowAnalyser backward (must (iotaVB,\p->genkill(genVB,killVB p) ))
iotaVB   = const S.empty 
genVB blk = freeArithmeticalExpressions blk
killVB p (Ass x a l) = S.filter (\a'->x `S.member` freeVariables a') (allArithmeticalExpressions p)
killVB p (MultAss asgs l) = S.unions [killVB p (Ass x a l) | (x,a)<-asgs]

livevariables          = createDataFlowAnalyser backward  (may  (iotaLV,\p-> genkill(genLV,killLV)) )
iotaLV p =  S.empty
genLV blk = freeVariables blk
killLV (Ass x a l) = S.singleton x
killLV (MultAss asgs l) = S.fromList $ (map fst) asgs
killLV _ = S.empty

stronglivevariables i  = createDataFlowAnalyser backward  (may (const i,\p->depgenkill(genSLV,const.killSLV)))
killSLV (Ass x _ _) = S.singleton x
killSLV (MultAss asgs _) = S.fromList (map fst asgs)
killSLV _           = S.empty

genSLV (Ass x a _) old  | x `S.member` old = freeVariables a 
                         | otherwise        = S.empty
genSLV (MultAss asgs _) old  = S.unions [freeVariables a | (x,a)<- lastAsgs asgs, x`S.member`old]
         where lastAsgs = foldr (\(x,a) acc -> if x `elem` (map fst acc) then acc else ((x,a):acc)) []
genSLV x _ = freeVariables x

