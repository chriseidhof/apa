{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Finals where

import BrownPLT.JavaScript.Syntax
import BrownPLT.JavaScript.Instances
import BrownPLT.JavaScript.Parser (parseScriptFromString) -- testing
import Control.Monad.State.Lazy
import Control.Applicative
import Data.Traversable hiding (sequence)
import Label
import qualified Data.Set as S
import Prelude hiding (init)
import Data.List (nub)
import qualified Data.Map as M
import Data.Generics (listify, Data)
import Text.ParserCombinators.Parsec (SourcePos (..))
import qualified DataFlowAnalysis.Program as P
import DataFlowAnalysis.DataFlowAnalyser
import Types
import DataFlowAnalysis.SemiLattice
import DataFlowAnalysis.Analysis
import SourcePos

test = case parseScriptFromString "" "if (true) { 5; } else { x = 5;y = x;}" of
            Left e  -> print e
            Right x -> case (label x) of
                            (Script a s) -> do
                              print $ Script a s
                              print $ P.labels (Script a s)
                              print $ analyze ana (Script a s)
                              -- print (Script a s)
                              -- let f = flow (BlockStmt a s)
                              -- print $ finals (BlockStmt a s)
                              -- print f

ana = createDataFlowAnalyser forward (createMeasureGen (const $ M.empty , coolFunction))

coolFunction :: JavaScript (Labeled SourcePosition) -> Label -> (Lattice -> Lattice)
coolFunction p = f
  where as  = map (\e@(AssignExpr a _ _ _) -> (labelOf a, e)) (assignments p)
        f lat = case lookup lat as of
                   Nothing -> id
                   Just (AssignExpr _ _ l r) -> \x -> M.insert (nameOf l) (typeOf x r) x

nameOf (VarRef _ (Id _ n)) = n
nameOf x                   = error $ "Assignment lhs only supports variable names: " ++ show x

type Assignment a = Expression a

instance (Show a) => P.Program (JavaScript (Labeled a)) where
  labels (Script a s) = nub $ map fst f ++ map snd f
                       where f = flow (BlockStmt a s)
  init  (Script a s)  = init (BlockStmt a s)
  final (Script a s)  = S.toList $ finals (BlockStmt a s)
  flow  (Script a s)  = flow (BlockStmt a s)

-- TODO this should be Program
class Finals f where
  finals      :: Show a => f (Labeled a) -> S.Set Label
  init        :: Show a => f (Labeled a) -> Label
  flow        :: Show a => f (Labeled a) -> Flow

assignments :: (Data a) => JavaScript a -> [Assignment a]
assignments = listify isAssignment
 where isAssignment (AssignExpr _ _ _ _) = True
       isAssignment _                    = False

type Flow = [(Label, Label)]

instance Finals Statement where
  finals (BlockStmt _ ls) = finals (last ls) -- todo: last is dangerous
  finals (EmptyStmt a) = l a
  finals (IfStmt _ cond e1 e2) = finals e1 `S.union` finals e2
  finals (WhileStmt _ cond body) = finals cond
  finals (ExprStmt a e) = finals e
  finals x = error $ "This statement is not supported yet: " ++ show x

  init (BlockStmt a ls) = labelOf a
  init (EmptyStmt a) = labelOf a
  init (IfStmt _ cond e1 e2) = init cond
  init (WhileStmt _ cond body) = init cond
  init (ExprStmt _ e) = init e
  init x = error $ "This statement is not supported yet: " ++ show x

  flow (BlockStmt a ls)        = flowList (labelOf a) ls ++ (concatMap flow ls)
  flow (EmptyStmt a)           = []
  flow (IfStmt _ cond e1 e2)   =  [(f, init e1)   | f <- S.elems (finals cond)] -- todo this can be more efficient probably
                               ++ [(f, init e2)   | f <- S.elems (finals cond)]
                               ++ flow cond
                               ++ flow e1
                               ++ flow e2
  flow (WhileStmt _ cond body) =  [(f, init body) | f <- S.elems (finals cond)] -- todo this can be more efficient probably
                               ++ [(b, init cond) | b <- S.elems (finals body)]
                               ++ flow cond
                               ++ flow body
  flow (ExprStmt _ e) = flow e
  flow x = error $ "This flow is not supported yet: " ++ show x

flowList :: (Show a, Finals f) => Label -> [f (Labeled a)] -> [(Label, Label)]
flowList _ []     = []
flowList x [y]    = [(x, init y)]
flowList x (y:ys) = concat [(x, init y):(flowList k ys) | k <- S.elems (finals y)]

instance Finals Expression where
  finals (StringLit a _)         = l a
  finals (NumLit a _)            = l a
  finals (IntLit a _)            = l a
  finals (BoolLit a _)           = l a
  finals (NullLit a)             = l a
  finals (AssignExpr a op l r)   = finals r
  finals (VarRef a _     )       = l a
  finals (InfixExpr _ op l r)    = finalsOp op l r
  finals (ListExpr _ ls)         = finals (last ls) -- todo: last is dangerous
  finals (ParenExpr _ e)         = finals e
  finals x                       = error $ "Finals not supported for: " ++ show x

  init (StringLit a _)      = labelOf a
  init (NumLit a _)         = labelOf a
  init (IntLit a _)         = labelOf a
  init (BoolLit a _)        = labelOf a
  init (NullLit a)          = labelOf a
  init (AssignExpr _ _ _ r) = init r
  init (VarRef a _     )    = labelOf a
  init (InfixExpr _ _ l _)  = init l
  init (ListExpr _ x)       = init $ head x
  init (ParenExpr _ e)      = init e
  init x                    = error $ "Init not supported for: " ++ show x

  flow (StringLit a _)         = []
  flow (NumLit a _)            = []
  flow (IntLit a _)            = []
  flow (BoolLit a _)           = []
  flow (NullLit a)             = []
  flow (AssignExpr a op l r)   = [(f, labelOf a) | f <- S.elems $ finals r] ++ flow r
  flow (VarRef a _     )       = []
  flow (InfixExpr _ op l r)    = [(f, init r) | f <- S.elems $ finals l] ++ flow l ++ flow r
  flow (ParenExpr _ e)         = flow e
  flow (ListExpr _ ls)         = flowList (init $ head ls) (tail ls) ++ concatMap flow ls


finalsOp :: (Show a) => InfixOp -> Expression (Labeled a) -> Expression (Labeled a) -> S.Set Label
finalsOp o l r | isLazyOp o  = finals l `S.union` finals r
               | otherwise   = finals r
  
isLazyOp OpMul  = False
isLazyOp OpSub  = False
isLazyOp OpLAnd = True
isLazyOp OpLOr  = True
isLazy   x      = error $ "Not implemented isLazy for " ++ x

l :: (Label, a) -> S.Set Label
l = S.singleton . labelOf

labelOf :: (Label, a) -> Label
labelOf = fst


-- TODO
--  finals (VarDeclStmt a decls) = undefined -- finals decls
--   finals (ReturnStmt a exp) = undefined
--   finals (FunctionStmt a name args body) = undefined
--  finals (IfSingleStmt a cond e1) = finals cond `S.union` finals e1	
-- SwitchStmt a (Expression a) [CaseClause a]	
-- DoWhileStmt a (Statement a) (Expression a)	
-- BreakStmt a (Maybe (Id a))	
-- ContinueStmt a (Maybe (Id a))	
-- LabelledStmt a (Id a) (Statement a)	
-- ForInStmt a (ForInInit a) (Expression a) (Statement a)	
-- ForStmt a (ForInit a) (Maybe (Expression a)) (Maybe (Expression a)) (Statement a)	
-- TryStmt a (Statement a) [CatchClause a] (Maybe (Statement a))	
-- ThrowStmt a (Expression a)	
-- WithStmt a (Expression a) (Statement a)	
--
--
--  finals (ThisRef a)             = finals a
--  finals (DotRef a parent child) = 
--  finals (ParenExpr a (Expression a)	
-- PostfixExpr a PostfixOp (Expression a)	
--  finals (ArrayLit a exs) = undefined -- finals exs	(but exs is a list...)
-- ObjectLit a [(Prop a, Expression a)]	
-- BracketRef a (Expression a) (Expression a)	
-- NewExpr a (Expression a) [Expression a]	
-- PrefixExpr a PrefixOp (Expression a)	
-- CondExpr a (Expression a) (Expression a) (Expression a)	
-- CallExpr a (Expression a) [Expression a]	
-- FuncExpr a [Id a] (Statement a)
-- RegexpLit a String Bool Bool	
--
