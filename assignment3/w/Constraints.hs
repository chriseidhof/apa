module Constraints where
import SemiLattice

data Constraint a b = a :< b
solve :: (Eq lat, SemiLattice lat) => [Int] -> [Constraint (Either Int lat) Int] -> [(Int, lat)]
solve vars const = fixpoint const (map (\x->(x,bottom)) vars)

fixpoint :: (Eq lat, SemiLattice lat) => [Constraint (Either Int lat) Int] -> [(Int, lat)] -> [(Int, lat)]
fixpoint c v = let v' = step c v in if (v==v') then v else fixpoint c v'

step :: (Eq lat, SemiLattice lat) => [Constraint (Either Int lat) Int] -> [(Int, lat)] -> [(Int, lat)]
step const v = foldr stepSingle v const

stepSingle :: (Eq lat, SemiLattice lat) => Constraint (Either Int lat) Int -> [(Int,lat)] -> [(Int,lat)]
stepSingle (Left x  :< var) val = val `with` (var, (val $$ var) \/ (val $$ x))
stepSingle (Right l :< var) val = val `with` (var, (val $$ var) \/ l)

($$) :: (Eq a,SemiLattice lat) => [(a,lat)] -> a -> lat
l $$ k = maybe bottom id (lookup k l)

with :: Eq a => [(a,b)]->(a,b)->[(a,b)]
[]          `with` (k,v) = [(k,v)]
((k',v'):kvs) `with` (k,v) 
   | k == k'    = (k,v):kvs
   | otherwise  = (k',v'):(kvs `with` (k,v))



