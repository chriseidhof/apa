module Chaotic where

import qualified Data.Set as S
import Control.Monad.State
import Data.Char (toLower)
import Data.List (union)
import Prelude hiding (init)

type Equations = ([Equation], [Equation])
type Equation = ([L], [L]) -> L -- (Entries, Exits)
type Variable = String
type FlowGraph = [(Label, Label)]
type L        = S.Set Variable
type Label    = Int

newtype BOp   = B (String, Bool -> Bool -> Bool)
newtype ROp   = R (String, Int  -> Int  -> Bool)
newtype AOp   = A (String, Int  -> Int  -> Int)

instance Show BOp where show (B x) = fst x
instance Show ROp where show (R x) = fst x
instance Show AOp where show (A x) = fst x

f :: Equations -> ([L], [L]) -> ([L], [L])
f (en, ex) x = (map ($x) en, map ($x) ex)

fixpoint f x = let step = f x in
               if step == x then x else fixpoint f step



data Stmt = Ass Variable AExp Label
          | Skip Label
          | Seq Stmt Stmt
--          | If BExp Label Stmt Stmt 
          | While BExp Label Stmt
data BExp = BVal Bool
--           | Not  BExp
--           | BOp BExp BOp BExp
          | ROp AExp ROp AExp
data AExp = Var Variable
          | AVal Int
          | AOp AExp AOp AExp

type StmtM = State Label Stmt

instance Show Stmt where
  show (Ass v e l) = block (v ++ " := " ++ show e) l
  show (Skip l)    = block (textt "skip") l
  show (Seq a b)   = show a ++ newline ++ show b
  show (While c l b) = textt "while " ++ block (show c) l ++ newline ++ indent (show b)

instance Show BExp where
  show (BVal t)  = textt $ map toLower (show t)
  show (ROp l o r) = unwords [show l, show o, show r]

instance Show AExp where
  show (Var v) = v
  show (AVal i) = show i
  show (AOp l o r) = unwords [show l, show o, show r]

block :: String -> Label -> String
block s l = "[" ++ s ++ "]^{" ++ show l ++ "}"

textt s = "\\textt{" ++ s ++ "}"

indent :: String -> String
indent = unlines . map ("\\;\\;" ++) . lines

newline = ";\\\\\n" 

label :: (Label -> a) -> State Label a
label f = do x <- get
             put (x + 1)
             return (f x)

skip :: StmtM
skip = label Skip

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
               ],
        skip
       ]

seqProgram :: [StmtM] -> StmtM
seqProgram = (liftM (foldr1 Seq)) . sequence

labelProgram :: StmtM -> Stmt
labelProgram = flip evalState 1

flowR :: Stmt -> FlowGraph
flowR s = [(l, l') | (l', l) <- flow s]

flow :: Stmt -> FlowGraph
flow (Ass _ _ _)       = []
flow (Skip _)          = []
flow (Seq s1 s2)       = unionL [flow s1, flow s2, [(l, init s2) | l <- final s1]]
flow (While cond l s)  = unionL [flow s, [(l, init s)], [(l', l) | l' <- final s]]

final :: Stmt -> [Label]
final (Ass _ _ l)       = [l]
final (Skip l)          = [l]
final (Seq s1 s2)       = final s2
final (While cond l s)  = [l]

init :: Stmt -> Label
init (Ass _ _ l)       = l
init (Skip l)          = l
init (Seq s1 s2)       = init s1
init (While cond l s)  = l

unionL = foldr1 union

ref :: Label -> Stmt -> Stmt
ref =  undefined
equations :: FlowGraph -> Stmt -> Equations
equations = undefined

analyze :: Stmt -> [(Label, L)]
analyze = error "TODO"
