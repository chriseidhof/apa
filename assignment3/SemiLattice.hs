{-# LANGUAGE FlexibleInstances #-}
module SemiLattice where

import qualified Data.Set as S
import Data.List

class SemiLattice l where
  bottom :: l
  (\/)   :: l -> l -> l  

join :: (SemiLattice l) => [l] -> l
join = foldr (\/) bottom 



class SetLike s where
   fromSet :: (Ord a) => S.Set a -> s a
   toSet   :: (Ord a) => s a     -> S.Set a

--there is something really unsatisfactory here
--but I feel we would need dependent types...

data SetUL a = SetUL{unSetUL:: S.Set a}        deriving (Eq)
data SetIL a = SetIL{unSetIL:: S.Set a} | Top  deriving (Eq)

instance SetLike SetUL where
    fromSet   = SetUL 
    toSet     = unSetUL
instance SetLike SetIL where
    fromSet   = SetIL
    toSet Top = error "toSet applied to Top"
    toSet x   = unSetIL x
     ---this is a partial function

instance (Ord a) => SemiLattice (SetUL a) where
  bottom   = fromSet S.empty
  s1 \/ s2 = fromSet (toSet s1 `S.union` toSet s2) 

instance (Ord a) => SemiLattice (SetIL a) where
  bottom   = Top
  Top\/ s  = s
  s  \/Top = s
  s1 \/ s2 = fromSet (toSet s1 `S.intersection` toSet s2) 

--show

instance (Show a,Ord a) => Show (SetUL a) where
   show = setToTeX show.toSet 
instance (Show a,Ord a) => Show (SetIL a) where
   show Top       = "\\top"
   show (SetIL s) = setToTeX show s



setToTeX showelemfun s = let x = commas s
                         in if null x then "\\emptyset" else "\\{" ++ x ++ "\\}"
    where commas = intercalate "," . map showelemfun . S.toList

