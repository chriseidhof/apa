module Program where

import Types
import Control.Monad.State

type StmtM = State Label Stmt

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

seqProgram :: [StmtM] -> StmtM
seqProgram = (liftM (foldr1 Seq)) . sequence

labelProgram :: StmtM -> Stmt
labelProgram = flip evalState 1
