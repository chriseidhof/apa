module Analysis where

import BrownPLT.JavaScript.Instances
import BrownPLT.JavaScript.Syntax
import DataFlowAnalysis.DataFlowAnalyser
import Finals
import Label
import SourcePos
import Types
import qualified Data.Map as M

ana = createDataFlowAnalyser forward (createMeasureGen (const M.empty , transferFunction))

transferFunction :: JavaScript (Labeled SourcePosition) -> Label -> (Lattice -> Lattice)
transferFunction p = f
  where as  = map (\e@(AssignExpr a _ _ _) -> (labelOf a, e)) (assignments p)
        f lab = case lookup lab as of
                   Nothing -> id
                   Just (AssignExpr _ _ l r) -> \x -> let t = typeOf x r in 
                                                      case toNameHierarchy l of
                                                         [n]    -> M.insert n t x
                                                         (n:ms) -> M.adjust (changeObject ms t) n x

-- TODO: this function is not total.
changeObject :: [String] -> [JsType] -> [JsType] -> [JsType]
changeObject []     newT = error "changeobject"
changeObject [x]    newT = undefined -- map (\curType -> curType {props = M.insert x newT (props curType)})
changeObject (x:xs) newT = undefined -- map (\curType -> curType {props = M.adjust (changeObject xs newT) x (props curType)})

toNameHierarchy (VarRef _ (Id _ n)) = [n]
toNameHierarchy (DotRef _ l (Id _ n)) = toNameHierarchy l ++ [n]
toNameHierarchy n = error $ "LHS of assignment not supported: " ++ show n
