{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import BrownPLT.JavaScript.Syntax
import Control.Monad.State.Lazy
import Label (Labeled)
import Control.Applicative
import qualified Data.Map as M
import DataFlowAnalysis.SemiLattice
import Data.List (nub)
import Control.Monad.Reader

data JsType = String | Numeral | Boolean | Function [JsType] JsType | Var TypeVar
 deriving (Show, Eq)
-- data Member  = M {key :: String, value :: JsType}
--  deriving Show
type TypeVar = Int

class Infer a where
  infer :: a -> StateT TypeVar (Reader Lattice) [JsType]

typeOf l x = runReader (evalStateT (infer x) 0) l

instance Infer InfixOp where
  infer OpLT         = numCond
  infer OpLEq	       = numCond
  infer OpGT	       = numCond
  infer OpGEq	       = numCond
  infer OpIn	       = error "not implemented yet"
  infer OpInstanceof = error "not implemented yet"
  infer OpEq	       = compCond
  infer OpNEq	       = compCond
  infer OpStrictEq	 = compCond
  infer OpStrictNEq	 = compCond
  infer OpLAnd	     = boolCond
  infer OpLOr	       = boolCond
  infer OpBAnd	     = boolCond
  infer OpBXor	     = boolCond
  infer OpBOr	       = boolCond
  infer OpMul	       = arith
  infer OpDiv	       = arith
  infer OpMod	       = arith
  infer OpSub	       = arith
  infer OpLShift	   = arith
  infer OpSpRShift   = arith
  infer OpZfRShift	 = arith
  infer OpAdd	       = arith 

instance Show a => Infer (Expression (Labeled a)) where
  infer (StringLit a _)         = return [String]
  infer (NumLit a _)            = return [Numeral]
  infer (IntLit a _)            = return [Numeral]
  infer (BoolLit a _)           = return [Boolean]
  infer (AssignExpr a op l r)   = infer r
  infer v@(VarRef a (Id _ n))   = do ctx <- ask
                                     case M.lookup n ctx of
                                          Nothing -> (pure . Var) <$> fresh
                                          Just x  -> return x
  infer (InfixExpr _ op l r)    = (pure . topLevel . head) <$>  infer op -- TODO
  infer (ListExpr _ ls)         = infer (last ls) -- todo: what's the semantics here?
  infer (ParenExpr _ e)         = infer e
  infer x                       = error $ "Infer not supported for: " ++ show x

numCond  = return [Function [Numeral, Numeral] Boolean]
compCond = do t <- fresh
              return [Function [Var t, Var t] Boolean]
boolCond = return [Function [Boolean, Boolean] Boolean]
arith    = return [Function [Numeral, Numeral] Numeral]
--str      = return [Function [String, String] String]
--

topLevel :: JsType -> JsType
topLevel (Function args res) = res
topLevel x = x

fresh :: (Monad m) => StateT TypeVar m TypeVar
fresh = do x <- get
           modify (+1)
           return x

type Lattice = M.Map String [JsType]

instance SemiLattice Lattice where
  bottom = M.empty
  a \/ b = M.unionWith (\x y -> nub (x ++ y)) a b
