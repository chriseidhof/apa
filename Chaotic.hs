module Chaotic where

import qualified Data.Set as S
import Control.Monad.State

type Equation = [L] -> L
type Variable = String
type L        = S.Set Variable
newtype BOp   = B (String, Bool -> Bool -> Bool)
newtype ROp   = R (String, Int  -> Int  -> Bool)
newtype AOp   = A (String, Int  -> Int  -> Int)
type Label    = Int

instance Show BOp where show (B x) = fst x
instance Show ROp where show (R x) = fst x
instance Show AOp where show (A x) = fst x

f :: [Equation] -> [L] -> [L]
f eqList x = map ($x) eqList

fixpoint f x = let step = f x in
               if step == x then x else fixpoint f step

data Stmt = Ass Variable AExp Label
          | Skip Label
          | Seq Stmt Stmt
          | If BExp Label Stmt Stmt 
          | While BExp Label Stmt
          deriving Show
data BExp = BVal Bool
          | Not  BExp
          | BOp BExp BOp BExp
          | ROp AExp ROp AExp
          deriving Show
data AExp = Var Variable
          | AVal Int
          | AOp AExp AOp AExp
          deriving Show

type StmtM = State Label Stmt

label :: (Label -> a) -> State Label a
label f = do x <- get
             put (x + 1)
             return (f x)

(=:) :: String -> AExp -> StmtM
v =: e = label (Ass v e)

while :: BExp -> [StmtM] -> StmtM
while cond body = do w <- label (While cond) 
                     b <- seqProgram body
                     return (w b)

begin = seqProgram

(>!) :: AExp -> AExp -> BExp
a >! b = ROp a (R (">", (>) )) b

(*!) :: AExp -> AExp -> AExp
a *! b = AOp a (A ("*", (*) )) b

(-!) :: AExp -> AExp -> AExp
a -! b = AOp a (A ("-", (-) )) b

infixl 7 *!
infixl 6 -!
infixl 4 >!
infixl 2 =:

prog :: StmtM
prog = begin
       ["r" =: AVal 1,
        "a" =: Var "r" *! Var "r",
        while (Var "y" >! AVal 1) [ 
               "r" =: Var "r" *! Var "x",
               "y" =: Var "y" -! AVal 1
               ]
       ]

seqProgram :: [StmtM] -> StmtM
seqProgram = (liftM (foldr1 Seq)) . sequence


labelProgram :: StmtM -> Stmt
labelProgram = flip evalState 1
