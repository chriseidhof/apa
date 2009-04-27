module Label where

import BrownPLT.JavaScript.Syntax
import BrownPLT.JavaScript.Instances
import Control.Monad.State.Lazy
import Control.Applicative
import Data.Traversable hiding (sequence)

-- TODO cleanup below
import SourcePos
import Text.ParserCombinators.Parsec.Pos (SourcePos)

-- Utilities 
type Label = Int
type Labeled a = (Label, a)

instance Applicative (State a) where
  (<*>) = ap
  pure  = return

refBuiltInObject = 1
refBuiltInFunction = 2
-- refBuiltInString 
-- ...
startLabels = 10

label :: JavaScript a -> JavaScript (Label, SourcePosition)
label (Script a x) = flip evalState startLabels $ do 
          x' <- sequence $ map (sequenceA . fmap ann) x
          a' <- ann a
          return $ Script a' x'

freshLabel :: State Label Label
freshLabel = do x <- get
                put (x + 1)
                return x

ann :: a -> State Label (Label, SourcePosition)
ann a = do x <- freshLabel
           return (x, transformSourcePos a)

labelOf :: (Label, a) -> Label
labelOf = fst
