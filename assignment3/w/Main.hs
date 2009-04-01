{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Control.Monad.State
import Types
import Utils
import Language
import Unify
import Solver

w :: Context AVar -> Expr () -> W (Expr (Type AVar), Subst AVar)
w gamma ex@(CInt _ _)      = do beta <- freshAVar
                                return ((Int  beta) <$> ex, id)
w gamma ex@(CBool _ _)     = do beta <- freshAVar
                                return ((Bool beta) <$> ex, id)
w gamma ex@(Var _ v)       = return ((lookup' v gamma) <$> ex, id)
w gamma ex@(Fn _ x e)      = do alpha <- freshTVar
                                beta  <- freshAVar
                                (e', theta) <- w (gamma @-> (x, alpha)) e
                                let tau     = tla e'
                                    fType   = theta alpha --> tau
                                addConstraints [beta :< alpha, beta :< tau]
                                return (Fn (fType beta) x e', theta)
w gamma ex@(Fun _ f x e)   = do ax <- freshTVar
                                a0 <- freshTVar
                                b  <- freshAVar
                                (e', theta0) <- w (gamma @-> (f, (ax --> a0) b) @-> (x, ax)) e
                                let tau = tla e'
                                    theta1 = unify tau (theta0 a0)
                                    fType = (theta1 (theta0 ax) --> theta1 tau)
                                addConstraints [b :< ax, b :< tau]
                                return (Fun (fType b) f x e', theta1 . theta0)
w gamma (App _ e1 e2)   =    do (e1', theta1) <- w gamma e1
                                (e2', theta2) <- w (theta1 <.> gamma) e2
                                alpha <- freshTVar
                                beta  <- freshAVar
                                let (tau1, tau2) = (tla e1', tla e2')
                                    fType  = tau2 --> alpha
                                    theta3 = unify (theta2 tau1) (fType beta)
                                return (App (theta3 alpha) e1' e2', theta3 . theta2 . theta1)
w gamma (If _ e0 e1 e2) =    do (e0', th0) <- w (                gamma) e0
                                (e1', th1) <- w (        th0 <.> gamma) e1
                                (e2', th2) <- w (th1 <.> th0 <.> gamma) e2
                                beta       <- freshAVar
                                let [t0, t1, t2] = map tla [e0', e1', e2']
                                addConstraints [beta :< t2]
                                let  th3  =  unify (th2 (th1 t0)) (Bool beta)
                                     th4  =  unify (th3 t2) (th3 (th2 t1))
                                return (If (th4 (th3 t2)) e0' e1' e2', th4 . th3 . th2 . th1 . th0)
w gamma (Let _ x e1 e2) =    do (e1', th1) <- w gamma e1
                                let t1 = tla e1'
                                (e2', th2) <- w ((th1 <.> gamma) @-> (x, t1))  e2
                                let t2 = tla e2'
                                return (Let t2 x e1' e2', th2 . th1)
w gamma (Op _ e1 op e2) =    do (e1', th1) <- w gamma e1
                                (e2', th2) <- w (th1 <.> gamma) e2
                                beta       <- freshAVar
                                let [t1, t2] = map tla [e1', e2']
                                let  th3  =  unify (th2 t1) (opTypeL op @@ beta)
                                     th4  =  unify (th3 t2) (opTypeR op @@ beta)
                                return (Op (opType op @@ beta) e1' op e2', th4 . th3 . th2 . th1)


-- A Function from t1 to t2
(-->) :: Type a -> Type a -> a -> Type a
t1 --> t2 = Function t1 t2

lookup' x (C gamma) = maybe (error $ "No such variable: " ++ show x) id $ lookup x gamma

-- Extend the context
(C gamma) @-> x = C (x:gamma)

-- State functions
freshTVar :: W (Type a)
freshTVar = do x <- get
               put x {tVar = tVar x + 1}
               return (TVar (tVar x))

freshAVar :: W AVar
freshAVar = do x <- get
               put x {aVar = A (a (aVar x) + 1)}
               return (aVar x)

addConstraints :: [Constraint AVar] -> W ()
addConstraints ls = modify (\x -> x {constraints = Constraints (ls ++ cs (constraints x))})

type InitialAnnotations = [(AVar, Annot)]

addInitialVariable :: Var -> Annot -> (Context AVar, InitialAnnotations) -> W (Context AVar, InitialAnnotations)
addInitialVariable var ann  (c,i) = do
  alpha <- freshTVar
  beta  <- freshAVar
  addConstraints [beta :< alpha]
  return (c @-> (var, alpha), (beta,ann):i)
  

-- Helpers
runW :: [(Var, Annot)] -> Expr () -> Expr (Type Annot)
runW init x = 
    let xs = map (uncurry addInitialVariable) init
        result = do (c, i)     <- foldl (>>=) (return (C [], [])) xs
                    (e, subst) <- w c x
                    return (i, e, subst)
        ((i, e, subst), st) = runState result (St 0 (A 0) (Constraints []))
        e'        = fmap subst e 
        css       = map (\(x :< y) -> (x, tla y)) $ cs (subst <.> constraints st)
        vars      = [0..a (aVar st)]
        solved    = solve i vars (tla $ tla e') css
        replace x = maybe (error "No polymorphism allowed") id $ lookup (a x) solved
    in fmap (fmap replace) e'

-- Example programs
ex01  = (fn 'x' (fn 'y' $ 'x') <@> i 2) <@> i 3
ex02  = let_ 'i' (fn 'x' 'x') $ let_ 'y' ('i' <@> i 2) ('i' <@> i 3)
ex03  = ((fn 'f' (fn 'x' $ 'f' <@> 'x')) <@> (fn 'y' 'y')) <@> i 42

ex04  = fn 'x' 'x' <@> ('z' +: i 1)
ex01'  = (fn 'x' (fn 'y' $ 'x') <@> ('q' +: i 1)) <@> ('r' +: i 1)
[res01, res02, res03] = map (runW []) [ex01, ex02, ex03]


