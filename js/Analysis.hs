module Analysis where

import BrownPLT.JavaScript.Instances
import BrownPLT.JavaScript.Syntax
import DataFlowAnalysis.DataFlowAnalyser
import Finals
import Label
import SourcePos
import Types
import DataFlowAnalysis.Analysis (analyze, scan_analyze)
import DataFlowAnalysis.SemiLattice (bottom)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.List (intercalate)
import BrownPLT.JavaScript.Parser (parseScriptFromString) -- testing

refBuiltInObject = Ref 1
refBuiltInFunction = Ref 2
-- refBuiltInString 
-- ...

myTest x = case parseScriptFromString "" (x ++ ";;") of
              Left err -> error (show err)
              Right x  -> scan_analyze ana $ label x


ana = createDataFlowAnalyser forward (createMeasureGen (const bottom, transferFunction))

transferFunction :: JavaScript (Labeled SourcePosition) -> Label -> (Lattice -> Lattice)
transferFunction p = f
  where as  = map (\e@(AssignExpr a _ _ _) -> (labelOf a, e)) (assignments p) 
        ns  = map (\e@(NewExpr    a _ _  ) -> (labelOf a, e)) (news p)
        fs  = map (\e@(FuncExpr   a _ _  ) -> (labelOf a, e)) (functiondecls p)
        f lab = case lookup lab (as ++ ns ++ fs) of
                   Nothing -> id
                   Just (AssignExpr _ _ l r)  -> \gamma -> let t = typeOf gamma r in 
                                                            case toNameHierarchy l of
                                                              [n]    -> gamma {types = M.insert n t (types gamma)}
                                                              (n:ms) -> let addrs = maybe [] id $ M.lookup n (types gamma) 
                                                                        in  gamma {refs  = compose [changeRefs addr ms t | (Reference addr) <- addrs] (refs gamma)}
                   Just (NewExpr  a clas args) -> \gamma -> gamma {refs = M.insert (Ref $ labelOf a) (newObject (constructorName clas) gamma) (refs gamma)} 
                   Just (FuncExpr a args body) -> let fref        = (Ref . labelOf) a
                                                      pref        = (Ref . negate . labelOf) a
                                                      script      = Script a [body]
                                                      anaResult   = snd (analyze ana script)
                                                      (ReturnStmt retLab (Just retExp)) = head'' (body) $ returns script -- todo
                                                      retContext  = fromJust $ M.lookup (labelOf retLab) anaResult
                                                      returnType  = typeOf retContext retExp
                                                      -- TODO local variables
                                                      baseType    = Function (map toName args) returnType
                                                  in \gamma -> gamma {refs =( M.insert fref (newFunction pref baseType) 
                                                                            . M.insert pref newPrototype 
                                                                            ) (refs gamma) }

compose = foldr (.) id

toName (Id _ x) = x

-- todo
head'' _ (x:xs) = x
head'' e _      = error $ show e

newObject :: String -> Lattice -> Object
newObject clas gamma = Object { valueType = base clas , props = M.empty , prototype = protOf} --TODO PROTOTYPE clas.prototype `mplus` Object.prototype
   where base "String"  = Just $ Left String
         base "Number"  = Just $ Left Numeral
         base "Boolean" = Just $ Left Boolean
         base _         = Nothing
         protOf = let classobjsaddrs = (maybe [] id $ M.lookup clas (types gamma) ) :: [JsType]
                      classobjs      = [M.lookup classobjaddr (refs gamma) | Reference classobjaddr <- classobjsaddrs] :: [Maybe Object]
                      prots          = concat [maybe [Reference refBuiltInObject]  id $ M.lookup "prototype" (props classobj) | Just classobj <- classobjs]
                  in case prots of [Reference x] -> Just x
                                   []            -> Nothing
                                   _             -> error $ "newObject ("++clas++") : multiple prototypes. context:" ++ show (gamma, classobjsaddrs, classobjs, prots)

newFunction :: Ref -> FunctionType -> Object
newFunction protRef baseType = Object {valueType = Just (Right baseType), prototype = Just refBuiltInFunction, props = M.singleton "prototype" [Reference protRef]}

newPrototype :: Object
newPrototype = Object {valueType = Nothing, prototype = Just refBuiltInObject, props = M.empty}

-- TODO: this function is not total.
changeRefs :: Ref -> [String] -> [JsType] -> References -> References
changeRefs ref []     newT ctx = error "changerefs"
changeRefs ref [x]    newT ctx = M.adjust (\obj -> obj {props = M.insert x newT $ props obj}) ref ctx
changeRefs ref (x:xs) newT ctx = let obj   = M.lookup ref ctx
                                     addrs = maybe [] (maybe [] id . M.lookup x . props) obj
                                 in  compose [changeRefs addr xs newT | (Reference addr) <- addrs] ctx


constructorName x = case toNameHierarchy x of
                         [n] -> n
                         x -> error $ "constructorname error: " ++ intercalate "." x

toNameHierarchy (VarRef _ (Id _ n)) = [n]
toNameHierarchy (DotRef _ l (Id _ n)) = toNameHierarchy l ++ [n]
toNameHierarchy n = error $ "LHS of assignment not supported: " ++ show n

fromJust' _ (Just x) = x
fromJust' e Nothing  = error $ show e
