module Solver (solve, Annot (..)) where

import qualified Constraints as C
import BTAnnotations
import Types (AVar, a)
import Data.List (nub, sort)

solve :: [(AVar, Annot)] -> [Int] -> AVar -> [(AVar, AVar)] -> [(Int, Annot)]
solve is vars topLevel ls = C.solve vars $ is' ++ ((Right D C.:< (a topLevel)) :  map (\(x,y) -> ((Left $ a x) C.:< a y)) ls)
 where is' = map (\(v, ann) -> Right ann C.:< a v) is
