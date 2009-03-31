{-# LANGUAGE FlexibleContexts #-}
module Unify where

import Types

-- Unifying

class Unifiable a where
  unify' :: a -> a -> Subst a

instance Unifiable AVar where
  unify' = (||->)

unify :: (Show (Type a), Unifiable a) => Type a -> Type a -> Subst a 
unify (Int  a) (Int a')                        = unify' a a'
unify (Bool a) (Bool a')                       = unify' a a'
unify (Function t1 t2 a) (Function t1' t2' a') = let theta0 = unify' a a'
                                                     theta1 = unify (theta0 t1) (theta0 t1')
                                                     theta2 = unify (theta1 $ theta0 t2) (theta1 $ theta0 t2')
                                                 in  theta2 . theta1 . theta0
unify (TVar a) (TVar b) | a == b = id
unify t        (TVar a) | a `elem` (variables t) = error $ "Infinite type: " ++ show a ++ " occurs in " ++ show t
                        | otherwise = a |-> t
unify (TVar a) t        = unify t (TVar a)
unify t1       t2       = error $ "Can not unify" ++ show t1 ++ " and " ++ show t2

(||->) :: AVar -> AVar -> Subst AVar
(||->) v v' = fmap (\x -> if x == v then v' else x)

(|->) :: TVar -> Type a -> Subst a
(|->) v t (TVar x) | v == x   = t
(|->) v t (Function x y a)    = Function ((v |-> t) x) ((v |-> t) y) a
(|->) _ _ x                   = x

variables :: Type a -> [TVar]
variables (TVar x)            = [x]
variables (Function x y a)    = variables x ++ variables y
variables _                   = []
