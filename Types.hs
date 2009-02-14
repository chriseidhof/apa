module Types where

import Data.Char (toLower)
import qualified Data.Set as S

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

instance Show Stmt where
  show (Ass v e l) = texBlock (v ++ " := " ++ show e) l
  show (Skip l)    = texBlock (textt "skip") l
  show (Seq a b)   = show a ++ newline ++ show b
  show (While c l b) = textt "while " ++ texBlock (show c) l ++ newline ++ indent (show b)

instance Show BExp where
  show (BVal t)  = textt $ map toLower (show t)
  show (ROp l o r) = unwords [show l, show o, show r]

instance Show AExp where
  show (Var v) = v
  show (AVal i) = show i
  show (AOp l o r) = unwords [show l, show o, show r]

texBlock :: String -> Label -> String
texBlock s l = "[" ++ s ++ "]^{" ++ show l ++ "}"

textt s = "\\textt{" ++ s ++ "}"

indent :: String -> String
indent = unlines . map ("\\;\\;" ++) . lines

newline = ";\\\\\n" 

class FreeVariables a where
  freeVariables :: a -> L

instance FreeVariables Stmt where
  freeVariables (Ass _ a _)   = freeVariables a
  freeVariables (While b _ _) = freeVariables b
  freeVariables _             = S.empty

instance FreeVariables AExp where
  freeVariables (Var v)     = S.singleton v
  freeVariables (AOp l _ r) = S.union (freeVariables l) (freeVariables r)
  freeVariables _           = S.empty

instance FreeVariables BExp where
  freeVariables (ROp l _ r) = S.union (freeVariables l) (freeVariables r)
