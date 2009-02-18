module SemiLattice where

import qualified Data.Set as S



class SemiLattice l where
  bottom :: l
  (\/)   :: l -> l -> l  

join :: (SemiLattice l) => [l] -> l
join = foldr (\/) bottom 


--there is already a class PartialOrd but its implementation is not
--really satisfactory because it treats in a non needed special manner imcomparable elements
class PartialOrder a where 
  --minimal definition
  lte:: a -> a -> Bool
  --functions
  cmp :: a -> a -> Maybe Ordering
  gte :: a -> a -> Bool
  eq  :: a -> a -> Bool
  slt :: a -> a -> Bool
  sgt :: a -> a -> Bool
  cmp a b | a `lte` b && b `lte` a = Just EQ
          | a `lte` b              = Just LT
          | b `lte` a              = Just GT
          | otherwise              = Nothing   
  a `gte` b = b `lte` a
  a `eq`  b = cmp a b == Just EQ
  a `slt` b = cmp a b == Just LT    
  a `sgt` b = cmp a b == Just GT

(<<=) :: (PartialOrder a) => a -> a -> Bool
(<<=) = lte

instance (Eq l, SemiLattice l) => PartialOrder l where
   l1 `lte` l2 = l1 \/ l2 == l2

-- Some SemiLattices

---- Set SemiLattices

newtype SetUL a = SetUL (S.Set a)

instance (Ord a) => SemiLattice (SetUL a) where
  bottom               = SetUL S.empty
  SetUL s1 \/ SetUL s2 = SetUL (s1 `S.union` s2)


data SetIL a = Top | SubSet (S.Set a)

instance (Ord a) => SemiLattice (SetIL a) where
   bottom      = Top 
   Top \/ s    = s
   s   \/ Top  = s
   (SubSet s1) \/ (SubSet s2) = SubSet (s1 `S.intersection` s2)

