{-# LANGUAGE FlexibleInstances #-}
module Language where

import Types

i = CInt ()
fn x b = Fn () x (toExpr b)
rec f x e = Fun () f x (toExpr e)
x <@> y = App () (toExpr x) (toExpr y)
if_ c l r = If () (toExpr c) (toExpr l) (toExpr r)
let_ x y b = Let () x (toExpr y) (toExpr b)
e1 +: e2 = Op () (toExpr e1) Plus (toExpr e2)
e1 >=: e2 = Op () (toExpr e1) GTE (toExpr e2)

class ToExpr a where
  toExpr :: a -> Expr ()

instance ToExpr (Expr ())  where toExpr = id
instance ToExpr Char       where toExpr = Var ()
instance ToExpr Bool       where toExpr = CBool ()

-- Example programs
ex01 = fn 'x' $ fn 'y' $ if_ ('x' >=: i 0) True 'y'
ex02 = (fn 'x' (fn 'y' $ 'x') <@> True) <@> i 7
