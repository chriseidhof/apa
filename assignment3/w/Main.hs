module Main where

import Control.Monad.State
import Utils

data Type ann = Int ann | Bool ann | Function (Type ann) (Type ann) ann | TVar TVar

instance (Show ann) => Show (Type ann) where
  show (Int ann) = "Int"
  show (Bool ann) = "Bool"
  show (Function l r a) = "(" ++ show l ++ ") -> (" ++ show r ++ ")"
  show (TVar v) = "t_" ++ show v

data Ann = S | D | AVar Int

type Var = Char
type TVar = Int
type AVar = Int

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
data St = St {tVar :: TVar, aVar :: AVar}

type Subst a = Type a -> Type a
type Context a = [(Var, Type a)]

f <$> x = fmap (const f) x

w :: Context () -> Expr () -> W (Expr (Type ()), Subst ())
w gamma ex@(CInt _ _)      = return ((Int  ()) <$> ex, id)
w gamma ex@(CBool _ _)     = return ((Bool ()) <$> ex, id)
w gamma ex@(Var _ v)       = return ((lookup' v gamma) <$> ex, id)
w gamma ex@(Fn _ x e)      = do alpha <- freshTVar
                                (e', theta) <- w (gamma @-> (x, alpha)) e
                                let tau = ann e'
                                return (Fn (theta alpha --> tau) x e', theta)
w gamma ex@(Fun _ f x e)   = do ax <- freshTVar
                                a0 <- freshTVar
                                (e', theta0) <- w (gamma @-> (f, ax --> a0) @-> (x, ax)) e
                                let tau = ann e'
                                let theta1 = unify tau (theta0 a0)
                                return (Fun (theta1 (theta0 ax) --> theta1 tau) f x e', theta1 . theta0)
w gamma (App _ e1 e2)   = do (e1', theta1) <- w gamma e1
                             (e2', theta2) <- w (theta1 <.> gamma) e2
                             let (tau1, tau2) = (ann e1', ann e2')
                             alpha <- freshTVar
                             let theta3 = unify (theta2 tau1) (tau2 --> alpha)
                             return (App (theta3 alpha) e1' e2', theta3 . theta2 . theta1)
w gamma (If _ e0 e1 e2) = do (e0', th0) <- w (                gamma) e0
                             (e1', th1) <- w (        th0 <.> gamma) e1
                             (e2', th2) <- w (th1 <.> th0 <.> gamma) e2
                             let [t0, t1, t2] = map ann [e0', e1', e2']
                             let  th3  =  unify (th2 (th1 t0)) (Bool ())
                                  th4  =  unify (th3 t2) (th3 (th2 t1))
                             return (If (th4 (th3 t2)) e0' e1' e2', th4 . th3 . th2 . th1 . th0)
w gamma (Let _ x e1 e2) = do (e1', th1) <- w gamma e1
                             let t1 = ann e1'
                             (e2', th2) <- w ((th1 <.> gamma) @-> (x, t1))  e2
                             let t2 = ann e2'
                             return (Let t2 x e1' e2', th2 . th1)
w gamma (Op _ e1 op e2) = do (e1', th1) <- w gamma e1
                             (e2', th2) <- w (th1 <.> gamma) e2
                             let [t1, t2] = map ann [e1', e2']
                             let  th3  =  unify (th2 t1) (opTypeL op)
                                  th4  =  unify (th3 t2) (opTypeR op)
                             return (Op (opType op) e1' op e2', th4 . th3 . th2 . th1)

opTypeL = fst3 . opType'
opTypeR = snd3 . opType'
opType  = thd3 . opType'

opType' Plus = (Int (), Int (), Int  ())
opType' GTE  = (Int (), Int (), Bool ())

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

freshTVar :: W (Type a)
freshTVar = do x <- get
               put x {tVar = tVar x + 1}
               return (TVar (tVar x))
-- helpers
--
runW e = let (t, s) = evalState (w [] e) (St 0 0)
         in (fmap s t)

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

ann :: Expr a -> a
ann (CInt  x c)        = x
ann (CBool x b)        = x
ann (Var   x v)        = x
ann (Fn    x v e)      = x
ann (Fun   x v1 v2 e)  = x
ann (App   x e1 e2)    = x
ann (If    x e1 e2 e3) = x
ann (Let   x v e1 e2)  = x
ann (Op    x e1 op e2) = x

-- Expression Language


i = CInt ()
b = CBool ()
v = Var ()
fn = Fn ()
rec f x e = Fun () f x e
x @ y = App () x y
if_ = If ()
let_ = Let ()
e1 +: e2 = Op () e1 Plus e2
e1 >=: e2 = Op () e1 GTE e2
