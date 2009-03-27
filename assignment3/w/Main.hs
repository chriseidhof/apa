{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Control.Monad.State

data Type ann = Int ann | Bool ann | Function (Type ann) (Type ann) ann | TVar TVar
 deriving (Show)

data Ann = S | D | AVar Int

type Var = Char
type TVar = Int

data Op = Plus | GTE

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
w gamma (Fun f x e)  = do ax <- fresh
                          a0 <- fresh
                          (tau, theta0) <- w (gamma @-> (f, ax --> a0) @-> (x, ax)) e
                          let theta1 = unify tau (theta0 a0)
                          return (theta1 (theta0 ax) --> theta1 tau, theta1 . theta0)
w gamma (App e1 e2) = do (tau1, theta1) <- w gamma e1
                         (tau2, theta2) <- w (theta1 <.> gamma) e2
                         alpha <- fresh
                         let theta3 = unify (theta2 tau1) (tau2 --> alpha)
                         return (theta3 alpha, theta3 . theta2 . theta1)
w gamma (If e0 e1 e2) = do (t0, th0) <- w (                gamma) e0
                           (t1, th1) <- w (        th0 <.> gamma) e1
                           (t2, th2) <- w (th1 <.> th0 <.> gamma) e2
                           let  th3  =  unify (th2 (th1 t0)) (Bool ())
                                th4  =  unify (th3 t2) (th3 (th2 t1))
                           return (th4 (th3 t2), th4 . th3 . th2 . th1 . th0)
w gamma (Let x e1 e2) = do (t1, th1) <- w gamma e1
                           (t2, th2) <- w ((th1 <.> gamma) @-> (x, t1))  e2
                           return (t2, th2 . th1)
w gamma (Op e1 op e2) = do (t1, th1) <- w gamma e1
                           (t2, th2) <- w (th1 <.> gamma) e2
                           let  th3  =  unify (th2 t1) (opTypeL op)
                                th4  =  unify (th3 t2) (opTypeR op)
                           return (opType op, th4 . th3 . th2 . th1)

opTypeL = fst3 . opType'
opTypeR = snd3 . opType'
opType  = thd3 . opType'

opType' Plus = (Int (), Int (), Int  ())
opType' GTE  = (Int (), Int (), Bool ())

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

unify :: Type () -> Type () -> Subst ()
unify (Int  _) (Int _)                        = id
unify (Bool _) (Bool _)                       = id
unify (Function t1 t2 _) (Function t1' t2' _) = let theta1 = unify t1 t1'
                                                    theta2 = unify (theta1 t2) (theta1 t2')
                                                in  theta2 . theta1
unify (TVar a) (TVar b) | a == b = id
unify t        (TVar a) | a `elem` (variables t) = error $ "Infinite type: " ++ show a ++ " occurs in " ++ show t
                        | otherwise = a |-> t
unify (TVar a) t        = unify t (TVar a)
unify t1       t2       = error $ "Can not unify" ++ show t1 ++ " and " ++ show t2

lookup' x = maybe (error $ "No such variable: " ++ show x) id . lookup x

gamma @-> x = x:gamma

t1 --> t2 = Function t1 t2 ()

variables :: Type a -> [TVar]
variables (TVar x)            = [x]
variables (Function x y a)    = variables x ++ variables y
variables _                   = []


infixr <.>
(<.>) :: Subst a -> Context a -> Context a
s <.> g = map (\(v,t) -> (v, s t)) g

(|->) :: TVar -> Type a -> Subst a
(|->) v t (TVar x) | v == x   = t
(|->) v t (Function x y a)    = Function ((v |-> t) x) ((v |-> t) y) a
(|->) _ _ x                   = x

fresh :: W (Type a)
fresh = do x <- get
           modify (+1)
           return (TVar x)
-- helpers
--
runW e = let (t, s) = evalState (w [] e) 0
         in (s t)

