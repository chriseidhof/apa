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
import Label

data PrimitiveType = String | Numeral | Boolean | Null | Undefined
 deriving (Show, Eq)

newtype Ref = Ref {address :: Int}
 deriving (Show, Eq, Ord)

data JsType = Primitive PrimitiveType | Reference Ref
 deriving (Show, Eq)

data Object = Object {valueType :: JsType, props :: PropertyMap, prototype :: Maybe Ref} 

-- | Function [JsType] JsType | Var TypeVar
-- data Member  = M {key :: String, value :: JsType}
--  deriving Show
type TypeVar = Int
type PropertyMap = M.Map String [JsType]

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
  infer (StringLit a _)         = return [string]
  infer (NumLit a _)            = return [numeral]
  infer (IntLit a _)            = return [numeral]
  infer (BoolLit a _)           = return [boolean]
  infer (ObjectLit a props)     = do --props' <- mapM inferProp props -- todo this can be more efficient
                                     Reference (Ref $ labelOf a)
                                     -- return [object "Object" (M.fromList props') Nothing]

  infer (AssignExpr a op l r)   = infer r
  infer v@(VarRef a (Id _ n))   = do ctx <- asks types
                                     case M.lookup n ctx of
                                          Nothing -> return []
                                          Just x  -> return x
  infer (InfixExpr _ op l r)    = (map topLevel) <$>  infer op -- TODO
  infer (ListExpr _ ls)         = infer (last ls) -- todo: what's the semantics here?
  infer (ParenExpr _ e)         = infer e
  infer (DotRef a p  (Id _ n))  = do objectType <- infer p
                                     undefined -- TODO
                                     -- case objectType of
                                     --      [(Object prot _ props)] -> case M.lookup n props of
                                     --                                      Just t  -> return t
                                     --                                      Nothing -> return [Null] -- TODO lookup in the objects prototype
                                     --      [] -> return [] -- is this correct?
                                     --      t -> error $ "Invalid type: " ++ show p ++ " is not an object. (" ++ show t ++ ")"

  infer x                       = error $ "Infer not supported for: " ++ show x

inferProp (n,e) = do ts <- infer e
                     return (name n, ts)

numCond  = undefined -- return [Function [Numeral, Numeral] Boolean]
compCond = undefined -- do t <- fresh
              --return undefined -- [Function [Var t, Var t] Boolean]
boolCond = undefined -- return [Function [Boolean, Boolean] Boolean]
arith    = undefined -- return [Function [Numeral, Numeral] Numeral]
--str      = return [Function [String, String] String]
--

topLevel :: JsType -> JsType
topLevel = undefined
-- topLevel (Function args res) = res
-- topLevel x = x

name :: Prop a -> String
name (PropId _ (Id _ s)) = s

fresh :: (Monad m) => StateT TypeVar m TypeVar
fresh = do x <- get
           modify (+1)
           return x

type References = M.Map Ref Object

data Lattice = Lattice { types :: M.Map String [JsType]
                       , refs  :: References
                       }

instance SemiLattice Lattice where
  bottom = Lattice M.empty M.empty
  (Lattice t1 r1) \/ (Lattice t2 r2) = Lattice (M.unionWith mergeType t1 t2) (M.unionWith mergeObj r1 r2)

-- TODO: this probably needs to be more advanced.
mergeType l r = nub (l ++ r)

mergeObj  o1 o2 | (prototype o1 /= prototype o2) = error "mergeRefs: Prototypes don't match"
                | (valueType o1 /= valueType o2) = error "mergeRefs: Values don't match"
                | otherwise                      = Object { prototype = prototype o1
                                                          , valueType = valueType o1
                                                          , props     = M.unionWith mergeType (props o1) (props o2)
                                                          }

string     = Primitive String
boolean    = Primitive Boolean
numeral    = Primitive Numeral
