module WhileProgram where

import WhileTypes
import WhileLanguage
import Control.Monad.State

type StmtM = State Label Stmt

label :: (Label -> a) -> State Label a
label f = do x <- get
             put (x + 1)
             return (f x)


begin :: [StmtM] -> StmtM
begin = seqProgram


var :: String -> AExp
var = Var . V

(=:) :: String -> AExp -> StmtM
v =: e = label (Ass (V v) e)

(==:) :: [String] -> [AExp] -> StmtM
v ==: e = label (MultAss (zip (map V v) e))

print :: AExp -> StmtM
print a = label (Print a)

skip :: StmtM
skip = label Skip

continue :: StmtM
continue = label Continue

break :: StmtM
break = label Break

ifte :: BExp -> ([StmtM],[StmtM]) -> StmtM
ifte cond (t,e) = do w <- label (If cond) 
                     tl <- seqProgram t
                     el <- seqProgram e
                     return (w tl el)

while :: BExp -> [StmtM] -> StmtM
while cond body = do w <- label (While cond) 
                     b <- seqProgram body
                     return (w b)


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

seqProgram :: [StmtM] -> StmtM
seqProgram = (liftM (foldr1 Seq)) . sequence

labelProgram :: StmtM -> Stmt
labelProgram = flip evalState 1
