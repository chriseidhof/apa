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
  show (Ass v e l) = block (v ++ " := " ++ show e) l
  show (MultAss asgs l) =  let (v,a) = (unzip asgs)
                           in  block (showlist v ++ " := " ++ showlist a) l
  show (Print a l)    = block (textt "print" ++ show a) l 
  show (Skip l)    = block (textt "skip") l
  show (Continue l)    = block (textt "continue") l
  show (Break l)    = block (textt "break") l 
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

showlist :: (Show a) => [a] -> String
showlist = (foldr1 (\x s -> x++","++s)).(map show) 
