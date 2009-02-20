module WhileLanguage where

import Data.Char (toLower)
import Data.List
import qualified Data.Set as S
import Control.Monad

import WhileTypes

--abstract syntax of annotated while programs


type Program   = Stmt

newtype BOp   = B (String, Bool -> Bool -> Bool)
newtype ROp   = R (String, Int  -> Int  -> Bool)
newtype AOp   = A (String, Int  -> Int  -> Int)

instance Eq AOp where
   A (x,_) == A (y,_) = x==y
instance Ord AOp where
   A (x,_) `compare` A (y,_) = x`compare`y



data AExp = Var Variable
          | AVal Int
          | AOp AExp AOp AExp
     deriving (Eq,Ord)
data BExp = BVal Bool
          | Not  BExp
          | BOp  BExp BOp BExp
          | ROp  AExp ROp AExp
data Stmt = Ass Variable AExp Label
          | MultAss [(Variable,AExp)] Label
          | Print AExp Label
          | Skip Label
          | Continue Label
          | Break Label
          | Seq Stmt Stmt
          | If BExp Label Stmt Stmt 
          | While BExp Label Stmt


--show to TeX (math environment) functions

instance Show BOp where show (B x) = fst x
instance Show ROp where show (R x) = fst x
instance Show AOp where show (A x) = fst x

instance Show Stmt where
  show (Ass v e l)      = texBlock (show v ++ " := " ++ show e) l
  show (MultAss asgs l) =  let (v,a) = (unzip asgs)
                           in  texBlock (showlist v ++ " := " ++ showlist a) l
  show (Print a l)      = texBlock (textt "print" ++ show a) l 
  show (Skip l)         = texBlock (textt "skip") l
  show (Continue l)     = texBlock (textt "continue") l
  show (Break l)        = texBlock (textt "break") l 
  show (Seq a b)        = show a ++ newline ++ show b
  show (If c l t e)     = textt "if"   ++ texBlock (show c) l ++ newline ++ 
                          textt "then" ++ newline ++ indent (show t) ++
                          textt "else" ++ newline ++ indent (show e) 
  show (While c l b)    = textt "while " ++ texBlock (show c) l ++ newline ++ indent (show b)

instance Show BExp where
  show (BVal t)    = textt $ map toLower (show t)
  show (BOp l o r) = unwords [show l, show o, show r]
  show (Not b)     = unwords ["\\neg", show b]
  show (ROp l o r) = unwords [show l, show o, show r]

instance Show AExp where
  show (Var v)     = show v
  show (AVal i)    = show i
  show (AOp l o r) = unwords [show l, show o, show r]

texBlock :: String -> Label -> String
texBlock s l = "[" ++ s ++ "]^{" ++ show l ++ "}"

textt s = "\\textt{" ++ s ++ "}"

indent :: String -> String
indent = unlines . map ("\\;\\;" ++) . lines

newline = ";\\\\\n" 

showlist :: (Show a) => [a] -> String
showlist = (foldr1 (\x s -> x++","++s)).(map show) 


--functions about statements structure

----constituent labels

labels :: Stmt -> [Label]
labels (Ass _ _ l)   = [l]
labels (MultAss _ l) = [l]
labels (Print _ l)   = [l]
labels (Skip l)      = [l]
labels (Continue l)  = [l]
labels (Break l)     = [l]
labels (Seq s1 s2)   = labels s1 ++ labels s2
labels (If _ l t e)  = [l] ++ labels t ++ labels e
labels (While _ l s) = [l] ++ labels s

----block pointed by label

block :: Stmt -> Label -> Maybe Stmt
block  b@(Ass _ _ l')   l  | l == l'   = Just b
block  b@(MultAss _ l') l  | l == l'   = Just b
block  b@(Print _ l')   l  | l == l'   = Just b
block  b@(Skip l')      l  | l == l'   = Just b
block  b@(Continue l')  l  | l == l'   = Just b
block  b@(Break l')     l  | l == l'   = Just b
block  b@(Seq s1 s2)    l              = block s1 l `mplus` block s2 l
block  b@(If _ l' t e)  l  | l == l'   = Just b
                           | otherwise = block t l `mplus` block e l
block  b@(While _ l' s) l  | l == l'   = Just b 
                           | otherwise = block s l
block _ _                              = Nothing

----variables in programs

vars :: Stmt -> S.Set Variable
vars (Ass x a _)      = S.singleton(x) `S.union` freeVariables a
vars (MultAss asgs _) = S.unions $ map (\(x,a)->S.singleton x `S.union` freeVariables a) asgs
vars (Print a _)      = freeVariables a
vars (Skip _)         = S.empty
vars (Continue _)     = S.empty
vars (Break _)        = S.empty
vars (Seq s1 s2)      = vars s1 `S.union` vars s2 
vars (If c _ t e)     = freeVariables c `S.union` vars t `S.union` vars e 
vars (While c _ s)    = freeVariables c `S.union` vars s 

----free variables in expressions
class FreeVariables a where
  freeVariables :: a -> S.Set Variable

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
  freeVariables (BVal b) = S.empty
  freeVariables (Not b)     = freeVariables b
  freeVariables (BOp l _ r) = S.union (freeVariables l) (freeVariables r)
  freeVariables (ROp l _ r) = S.union (freeVariables l) (freeVariables r)

----arithmetic expressions
class FreeAExp a where
  freeArithmeticalExpressions :: a -> S.Set AExp

instance FreeAExp Stmt where
  freeArithmeticalExpressions (Ass _ a _)     = freeArithmeticalExpressions a
  freeArithmeticalExpressions (MultAss asgs _)= S.unions $ map (freeArithmeticalExpressions . snd) asgs
  freeArithmeticalExpressions (Print a _)     = freeArithmeticalExpressions a
  freeArithmeticalExpressions _               = S.empty

allArithmeticalExpressions (Seq s1 s2)       = allArithmeticalExpressions s1 `S.union` allArithmeticalExpressions s2 
allArithmeticalExpressions (If b _ s1 s2)   = freeArithmeticalExpressions b `S.union`
                                              allArithmeticalExpressions s1 `S.union` allArithmeticalExpressions s2
allArithmeticalExpressions (While b _ s)   = freeArithmeticalExpressions b `S.union`allArithmeticalExpressions s 
allArithmeticalExpressions x = freeArithmeticalExpressions x

instance FreeAExp AExp where
  freeArithmeticalExpressions ae@(AOp l _ r) = S.singleton ae `S.union` (freeArithmeticalExpressions l) `S.union` (freeArithmeticalExpressions r)
  freeArithmeticalExpressions _           = S.empty

instance FreeAExp BExp where
  freeArithmeticalExpressions (BVal b) = S.empty
  freeArithmeticalExpressions (Not b)     = freeArithmeticalExpressions b
  freeArithmeticalExpressions (BOp l _ r) = S.union (freeArithmeticalExpressions l) (freeArithmeticalExpressions r)
  freeArithmeticalExpressions (ROp l _ r) = S.union (freeArithmeticalExpressions l) (freeArithmeticalExpressions r)


