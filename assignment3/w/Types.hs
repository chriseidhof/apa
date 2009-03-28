{-# LANGUAGE FlexibleInstances #-}
module Types where

import Control.Monad.State (State)
import Utils

data Type ann = Int ann | Bool ann | Function (Type ann) (Type ann) ann | TVar TVar

data Ann = S | D | AVar Int

type Var = Char
type TVar = Int
newtype AVar = A {a :: Int}
 deriving (Eq)
instance Show AVar where show (A x) = "b_" ++ show x

data Op = Plus | GTE
 deriving Show

data Expr a = CInt  a Int 
            | CBool a Bool
            | Var   a Var
            | Fn    a Var  (Expr a)
            | Fun   a Var Var  (Expr a)
            | App   a (Expr a) (Expr a)
            | If    a (Expr a) (Expr a)    (Expr a)
            | Let   a Var      (Expr a)    (Expr a)
            | Op    a          (Expr a) Op (Expr a)
            deriving (Show)

type W = State St
data St = St {tVar :: TVar, aVar :: AVar, constraints :: [Constraint]}

type Subst a = Type a -> Type a
type Context a = [(Var, Type a)]
data Constraint = AVar :< Type AVar deriving (Show)

opTypeL = fst3 . opType'
opTypeR = snd3 . opType'
opType  = thd3 . opType'

opType' Plus = (Int (), Int (), Int  ())
opType' GTE  = (Int (), Int (), Bool ())


-- Getting top-level annotations

class TopLevelAnnotiation f where
  tla :: f a -> a

instance TopLevelAnnotiation Expr where
  tla (CInt  x c)        = x
  tla (CBool x b)        = x
  tla (Var   x v)        = x
  tla (Fn    x v e)      = x
  tla (Fun   x v1 v2 e)  = x
  tla (App   x e1 e2)    = x
  tla (If    x e1 e2 e3) = x
  tla (Let   x v e1 e2)  = x
  tla (Op    x e1 op e2) = x

instance TopLevelAnnotiation Type where
  tla (Int ann)        = ann
  tla (Bool ann)       = ann
  tla (Function l r a) = a
  tla (TVar v)         = error $ "No top-level annotation for TVar(" ++ show v ++ ")"

-- Functor instances

instance Functor Expr where
  fmap f (CInt  x c)        = CInt (f x) c
  fmap f (CBool x b)        = CBool (f x) b
  fmap f (Var   x v)        = Var (f x) v
  fmap f (Fn    x v e)      = Fn (f x) v (fmap f e)
  fmap f (Fun   x v1 v2 e)  = Fun (f x) v1 v2 (fmap f e)
  fmap f (App   x e1 e2)    = App (f x) (fmap f e1) (fmap f e2)
  fmap f (If    x e1 e2 e3) = If (f x) (fmap f e1) (fmap f e2) (fmap f e3)
  fmap f (Let   x v e1 e2)  = Let (f x) v (fmap f e1) (fmap f e2)
  fmap f (Op    x e1 op e2) = Op (f x) (fmap f e1) op (fmap f e2)

instance Functor Type where
  fmap f (Int x) = Int (f x)
  fmap f (Bool x) = Bool (f x)
  fmap f (Function l r x) = Function (fmap f l) (fmap f r) (f x)
  fmap f (TVar x) = TVar x

-- Show instances

instance Show (Type ()) where
  show (Int ann) = "Int"
  show (Bool ann) = "Bool"
  show (Function l r a) = "[" ++ show l ++ " -> " ++ show r ++ "]"
  show (TVar v) = "t_" ++ show v

instance Show (Type AVar) where
  show (Int ann)        = show ann
  show (Bool ann)       = show ann
  show (Function l r a) = "(" ++ show l ++ "->" ++ show r ++ ")^" ++ show a
  show (TVar v) = "t_" ++ show v

-- Unifying

class Unifiable a where
  unify' :: a -> a -> Subst a
