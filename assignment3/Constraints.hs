module Constraints where

data Constraint a b = a :< b

solve :: (SemiLattice lat) => [Int] -> [Constraint (Either Int lat) Int] -> [(Int, lat)]
solve vars const = fixpoint const (map (\x->(x,bottom)) vars)

fixpoint :: (SemiLattice lat) => [Constraints (Either Int lat) Int] -> [(Int, lat)] -> [(Int, lat)]
fixpoint c v = let v' = step c v in if (v==v') then v else fixpoint c v'

step :: (SemiLattice lat) => [Constraints (Either Int lat) Int] -> [(Int, lat)] -> [(Int, lat)]
step const = foldr  stepSingle const

stepSingle :: (SemiLattice lat) => Constraint (Either Int lat) -> [(Int,lat)] -> [(Int,lat)]
stepSingle (Left x  :< var) val = val `where` (var,val $$ var \/ val $$ x)
stepSingle (Right l :< var) val = val `where` (var,val $$ var \/ l)

($$) = flip lookup


[] `where` (k,v) = [(k,v)]
[(k',v'):kvs] `where` (k,v) 
   | k == k'    = (k,v):kvs
   | otherwies  = (k',v'):(kvs `where` (k,v))

