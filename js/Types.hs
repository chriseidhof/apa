module Types where

import WebBits.JavaScript.Syntax
import Control.Monad.State.Lazy

data JsType = String | Numeral | Boolean 
 deriving Show
type TypeVar = Int

class Infer a where
  infer :: a -> State TypeVar [JsType]

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

numCond  = return [Function [Numeral, Numeral] Boolean]
compCond = do t <- fresh
              return [Function [Poly t, Poly t] Boolean]
boolCond = return [Function [Boolean, Boolean] Boolean]
arith    = return [Function [Numeral, Numeral] Numeral]

fresh :: State TypeVar TypeVar
fresh = do x <- get
           modify (+1)
           return x
