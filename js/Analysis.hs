module Analysis where

import BrownPLT.JavaScript.Instances
import BrownPLT.JavaScript.Syntax
import DataFlowAnalysis.DataFlowAnalyser
import Finals
import Label
import SourcePos
import Types
import DataFlowAnalysis.SemiLattice (bottom)
import qualified Data.Map as M

ana = createDataFlowAnalyser forward (createMeasureGen (const bottom, transferFunction))

transferFunction :: JavaScript (Labeled SourcePosition) -> Label -> (Lattice -> Lattice)
transferFunction p = f
  where as  = map (\e@(AssignExpr a _ _ _) -> (labelOf a, e)) (assignments p) 
        ns  = map (\e@(NewExpr    a _ _  ) -> (labelOf a, e)) (news p)
        f lab = case lookup lab (as ++ ns) of
                   Nothing -> id
                   Just (AssignExpr _ _ l r)  -> \gamma -> let t = typeOf gamma r in 
                                                            case toNameHierarchy l of
                                                              [n]    -> gamma {types = M.insert n t (types gamma)}
                                               --               (n:ms) -> gamma{types = M.adjust (changeObject ms t) n (types gamma)}
                   Just (NewExpr a clas args) -> \gamma -> gamma {refs = M.insert (Ref $ labelOf a) (newObject (constructorName clas) $ refs gamma) (refs gamma)} 

newObject :: String -> References -> Object
newObject clas refenv = Object { valueType = base clas , props = M.empty , prototype = Nothing} --TODO PROTOTYPE clas.prototype `mplus` Object.prototype
   where base "String"  = Just String
         base "Number"  = Just Numeral
         base "Boolean" = Just Boolean
         base _         = Nothing

-- TODO: this function is not total.
changeObject :: [String] -> [JsType] -> [JsType] -> [JsType]
changeObject []     newT = error "changeobject"
changeObject [x]    newT = undefined -- map (\curType -> curType {props = M.insert x newT (props curType)})
changeObject (x:xs) newT = undefined -- map (\curType -> curType {props = M.adjust (changeObject xs newT) x (props curType)})

-- todo: this is unsafe
constructorName = head . toNameHierarchy

toNameHierarchy (VarRef _ (Id _ n)) = [n]
toNameHierarchy (DotRef _ l (Id _ n)) = toNameHierarchy l ++ [n]
toNameHierarchy n = error $ "LHS of assignment not supported: " ++ show n
