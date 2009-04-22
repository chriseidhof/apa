module DataFlowAnalysis.MonotoneFramework where

import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map as M
import DataFlowAnalysis.Aux

import DataFlowAnalysis.SemiLattice


data MonotoneFramework lat = MF FlowInfo (MeasureInfo lat)

type Label = Int
type FlowGraph = [(Label,Label)]

type FlowInfo = ([Label],FlowGraph,[Label])
type MeasureInfo lat = (lat,M.Map Label (lat->lat))

type Equations       lat = (M.Map Label (Equation lat), M.Map Label (Equation lat))
type Equation        lat = IterationResult lat -> lat
type IterationResult lat = (M.Map Label lat, M.Map Label lat) -- (Opened, Closed)


vertices :: MonotoneFramework l -> [Label] 
vertices (MF (vs,_,_) (_,_)) = vs
edges    :: MonotoneFramework l -> FlowGraph
edges (MF (_,f,_) (_,_))  = f
extremes :: MonotoneFramework l -> [Label] 
extremes (MF (_,_,e) (_,_))  = e

iota     :: MonotoneFramework l -> l
iota (MF (_,_,_) (i,_))   = i
transf   :: MonotoneFramework l -> Label -> l -> l
transf (MF (_,_,_) (_,t)) = fromJust . (`M.lookup` t)



equations :: (SemiLattice lat) => MonotoneFramework lat -> Equations lat
equations mf = (M.fromAscList opened, M.fromAscList closed)
 where opened = map (\l -> (l, opened_eq mf l)) (vertices mf)
       closed = map (\l -> (l, closed_eq  mf l)) (vertices mf)


opened_val :: Label -> IterationResult lat -> lat
opened_val l = fromJust . M.lookup l . fst

closed_val :: Label -> IterationResult lat -> lat
closed_val l  = fromJust . M.lookup l . snd

opened_eq :: (SemiLattice lat) => MonotoneFramework lat -> Label -> Equation lat 
opened_eq mf label r  = join [closed_val l' r | (l', l) <- edges mf, l==label] \/ st
          where st | label `elem` extremes mf = iota mf
                   | otherwise                = bottom

closed_eq :: MonotoneFramework lat -> Label -> Equation lat
closed_eq mf label = transf mf label . opened_val label

seedEqs :: (SemiLattice lat)=> MonotoneFramework lat -> IterationResult lat
seedEqs mf = let bottomvector = M.fromAscList $ map (id`split`const bottom) (vertices mf)
             in (bottomvector,bottomvector)
