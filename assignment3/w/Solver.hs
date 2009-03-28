module Solver where

import qualified Constraints as C
import BTAnnotations
import Types (AVar, a)
import Data.List (nub, sort)

solve :: [Int] -> AVar -> [(AVar, AVar)] -> [(Int, Annot)]
solve vars topLevel ls = C.solve vars ((Right D C.:< (a topLevel)) : map (\(x,y) -> ((Left $ a x) C.:< a y)) ls)
