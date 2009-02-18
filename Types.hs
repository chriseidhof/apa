module Types where

import Data.Char (toLower)
import qualified Data.Set as S
import qualified Data.Map as M


type Equations       = (M.Map Label Equation, M.Map Label Equation)
type Equation        = IterationResult -> L
type Variable        = String
type FlowGraph       = [(Label, Label)]

type L               = S.Set Variable
type Label           = Int
type Program         = Stmt
type IterationResult = (M.Map Label L, M.Map Label L) -- (Entries, Exits)



newtype BOp   = B (String, Bool -> Bool -> Bool)
newtype ROp   = R (String, Int  -> Int  -> Bool)
newtype AOp   = A (String, Int  -> Int  -> Int)

instance Show BOp where show (B x) = fst x
instance Show ROp where show (R x) = fst x
instance Show AOp where show (A x) = fst x

data Stmt = Ass Variable AExp Label
          | MultAss [(Variable,AExp)] Label
          | Print AExp Label
          | Skip Label
          | Continue Label
          | Break Label
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
  show (MultAss asgs l) =  let (v,a) = (unzip asgs)
                           in  texBlock (showlist v ++ " := " ++ showlist a) l
  show (Print a l)    = texBlock (textt "print" ++ show a) l 
  show (Skip l)    = texBlock (textt "skip") l
  show (Continue l)    = texBlock (textt "continue") l
  show (Break l)    = texBlock (textt "break") l 
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
texBlock s l = "\\lbrack " ++ s ++ " \\rbrack^{" ++ show l ++ "}"

textt s = "\\texttt{" ++ s ++ "}"

indent :: String -> String
indent = unlines . map ("\\;\\;" ++) . lines

newline = ";\\\\\n" 

class FreeVariables a where
  freeVariables :: a -> L

instance FreeVariables Stmt where
  freeVariables (Ass _ a _)     = freeVariables a
  freeVariables (MultAss asgs _)= S.unions $ map (freeVariables . snd) asgs
  freeVariables (Print a _)     = freeVariables a
  freeVariables (While b _ _)   = freeVariables b
  freeVariables _               = S.empty

instance FreeVariables AExp where
  freeVariables (Var v)     = S.singleton v
  freeVariables (AOp l _ r) = S.union (freeVariables l) (freeVariables r)
  freeVariables _           = S.empty

instance FreeVariables BExp where
  freeVariables (ROp l _ r) = S.union (freeVariables l) (freeVariables r)
--  freeVariables (BOp l _ r) = S.union (freeVariables l) (freeVariables r)
--  freeVariables (Not b)     = freeVariables b
  freeVariables (BVal b) = S.empty

showlist :: (Show a) => [a] -> String
showlist = (foldr1 (\x s -> x++","++s)).(map show) 
