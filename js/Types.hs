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
import Debug.Trace (trace)
import Data.Maybe (isJust, fromJust)

data PrimitiveType = String | Numeral | Boolean | Null | TypeOf String | Undefined
 deriving (Show, Eq, Ord)

data Ref = Ref {address :: Int} | Cloned {clonePosition :: Int, originalAddress :: Ref}
 deriving (Show, Eq, Ord)

data FunctionType = Function { args       :: [String]
                             , resultType :: [JsType]
                             }
 deriving (Show, Eq, Ord)

data JsType = Primitive PrimitiveType | Reference Ref
 deriving (Show, Eq, Ord)

data Object = Object {valueType :: Maybe (Either PrimitiveType FunctionType), props :: PropertyMap, prototype :: Maybe Ref} 
 deriving (Show, Eq)

-- | Function [JsType] JsType | Var TypeVar
-- data Member  = M {key :: String, value :: JsType}
--  deriving Show
type TypeVar = Int
type PropertyMap = M.Map String [JsType]

class Infer a where
  infer :: a -> InferMonad [JsType]


type InferMonad a = StateT TypeVar (Reader Lattice) a

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
  infer (NewExpr a _ _ )        = return [Reference (Ref $ labelOf a)]

  infer (AssignExpr a op l r)   = infer r
  infer v@(VarRef a (Id _ n))   = inferVar n
  infer (InfixExpr _ op l r)    = (map topLevel) <$>  infer op -- TODO
  infer (ListExpr _ ls)         = infer (last ls) -- todo: what's the semantics here?
  infer (ParenExpr _ e)         = infer e
  infer (FuncExpr a args body)  = return [Reference (Ref $ labelOf a)]
  infer (CallExpr a f args)     = do ctx <- asks types
                                     lhs <- infer f
                                     argTypes <- mapM infer args
                                     typs <- mapM (inferCall (labelOf a) argTypes) lhs -- TODO: do we need all combinations?
                                     return $ concat typs

  infer (DotRef a p  (Id _ n))  = do objectType <- infer p
                                     refMap      <- asks refs
                                     -- todo: this can be more clearly
                                     let mUndefined = maybe [tUndefined]
                                         lookupInPrototype = mUndefined (lookupRef . Reference) . prototype
                                         lookupRef (Reference ref) = mUndefined f $ M.lookup ref refMap

                                         lookupRef _               = [tUndefined]
                                         f obj = maybe (lookupInPrototype obj) id $ M.lookup n $ props obj
                                     types' <- asks types
                                     return $ nub $ concatMap lookupRef objectType

  infer x                       = error $ "Infer not supported for: " ++ show x

inferProp (n,e) = do ts <- infer e
                     return (name n, ts)

inferVar n = do ctx <- asks types
                case M.lookup n ctx of
                     Nothing -> return [Primitive $ TypeOf n]
                     Just x  -> return x

numCond  = undefined -- return [Function [Numeral, Numeral] Boolean]
compCond = undefined -- do t <- fresh
              --return undefined -- [Function [Var t, Var t] Boolean]
boolCond = undefined -- return [Function [Boolean, Boolean] Boolean]
arith    = undefined -- return [Function [Numeral, Numeral] Numeral]
--str      = return [Function [String, String] String]
--

inferCall :: Label -> [[JsType]] -> JsType -> InferMonad [JsType]
inferCall lab args p@(Primitive (TypeOf _)) = return []
inferCall lab args p@(Primitive _) = error $ "Exception: inferCall: " ++ show p
inferCall lab args p@(Reference r) = do refMap <- asks refs
                                        case M.lookup r refMap of
                                             Nothing -> return [tUndefined]
                                             Just o  -> case valueType o of
                                                             Just (Right (Function argNames resultType)) -> do
                                                                let substList = safeZip argNames args
                                                                resultT <- mapM (subst lab substList) resultType
                                                                return $ concat resultT
                                                             _ -> error $ "Not a function type (inferCall)"

subst :: Label -> [(String, [JsType])] -> JsType -> InferMonad [JsType]
subst label substList (Primitive (TypeOf x)) | isJust l  = return $ fromJust l
                                             | otherwise = inferVar x
                                               where l = lookup x substList
subst label substList (Primitive p         ) = return [Primitive p]
subst label substList (Reference r)          = return [Reference (Cloned label r)]


safeZip [] [] = []
safeZip (x:xs) (y:ys) = (x,y):(safeZip xs ys)
safeZip _ _ = error "safeZip: lists of unequal length"

topLevel :: JsType -> JsType
topLevel = undefined
-- topLevel (Function args res) = res
-- topLevel x = x

fromId :: Id a -> String
fromId (Id _ s) = s

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
               deriving (Show, Eq)

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
tUndefined = Primitive Undefined
ref        = Reference . Ref
