{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Control.Monad.State

data Type ann = Int ann | Bool ann | Function (Type ann) (Type ann) ann | TVar TVar
 deriving (Show)

data Ann = S | D | AVar Int

type Var = Char
type TVar = Int

data Op = Plus | Minus

data Expr = CInt  Int 
          | CBool Bool
          | Var   Var
          | Fn    Var  Expr
          | Fun   Var Var Expr
          | App   Expr Expr
          | If    Expr Expr Expr
          | Let   Var Expr Expr
          | Op    Expr Op Expr

type W = State TVar

type Subst a = Type a -> Type a
type Context a = [(Var, Type a)]

w :: Context () -> Expr -> W (Type (), Subst ())
w gamma (CInt _)  = return (Int  ()        , id)
w gamma (CBool _) = return (Bool ()        , id)
w gamma (Var   v) = return (lookup' v gamma, id)
w gamma (Fn x e)  = do alpha <- fresh
                       (tau, theta) <- w (gamma @-> (x, alpha)) e
                       return (theta alpha --> tau, theta)
w gamma (App e1 e2) = do (tau1, theta1) <- w gamma e1
                         (tau2, theta2) <- w (subst theta1 gamma) e2
                         alpha <- fresh
                         let theta3 = unify (theta2 tau1) (tau2 --> alpha)
                         return (theta3 alpha, theta3 . theta2 . theta1)

unify :: Type () -> Type () -> Subst ()
unify (Int  _) (Int _)                        = id
unify (Bool _) (Bool _)                       = id
unify (Function t1 t2 _) (Function t1' t2' _) = let theta1 = unify t1 t1'
                                                    theta2 = unify (theta1 t2) (theta1 t2')
                                                in  theta2 . theta1
unify (TVar a) (TVar b) | a == b = id
unify t        (TVar a) | a `elem` (variables t) = error "Infinite type"
                        | otherwise = a |-> t
unify (TVar a) t        = unify t (TVar a)
unify t1       t2       = error $ "Can not unify" ++ show t1 ++ " and " ++ show t2

lookup' x = maybe (error $ "No such variable: " ++ show x) id . lookup x

(|->) :: TVar -> Type a -> Subst a
a |-> t = replace a t


gamma @-> x = x:gamma

t1 --> t2 = Function t1 t2 ()

variables :: Type a -> [TVar]
variables (TVar x)            = [x]
variables (Function x y a)    = variables x ++ variables y
variables _                   = []


subst :: Subst a -> Context a -> Context a
subst s gamma = map (\(v,t) -> (v, s t)) gamma

replace :: TVar -> Type a -> Type a -> Type a
replace v t (TVar x) | v == x   = t
replace v t (Function x y a)    = Function (replace v t x) (replace v t y) a
replace _ _ x                   = x

fresh :: W (Type a)
fresh = do x <- get
           modify (+1)
           return (TVar x)
-- helpers
--
runW e = let (t, s) = evalState (w [] e) 0
         in (s t)

