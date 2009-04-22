module Label where

import BrownPLT.JavaScript.Syntax
import BrownPLT.JavaScript.Instances
import Control.Monad.State.Lazy
import Control.Applicative
import Data.Traversable hiding (sequence)

-- Utilities 
type Label = Int
type Labeled a = (Label, a)

instance Applicative (State a) where
  (<*>) = ap
  pure  = return

label :: JavaScript a -> JavaScript (Label, a)
label (Script a x) = flip evalState 0 $ do 
          x' <- sequence $ map (sequenceA . fmap ann) x
          a' <- ann a
          return $ Script a' x'

freshLabel :: State Label Label
freshLabel = do x <- get
                put (x + 1)
                return x

ann :: a -> State Label (Label, a)
ann a = do x <- freshLabel
           return (x, a)
