module TableOutput where

import Data.List	
import qualified Data.Map as M

import MonotoneFramework


data Cell lat = Cell {name :: String, lattice :: lat}
data Row lat = Row String [lat]
  deriving Show
data Table lat = Table [Row lat]

resultToTable :: (String,Bool,String,String) -> [IterationResult lat] -> Table lat
resultToTable prtinfo = Table . map cellsToRow . transpose . map (formatIteration prtinfo)

cellsToRow cells = if allEqual (map name cells)
                      then Row (name (head cells)) (map lattice cells)
                      else error "not all cells have been grouped correctly"

instance (Show lat) => Show (Table lat) where
   show (Table rows) = "$\\begin{array}{llccccccccccccccccccccc}" ++
                       formatTable rows ++ "\\end{array}$"

formatTable :: (Show lat) => [Row lat] -> String
formatTable = intercalate " \\\\ \n" . map showRow
 
showRow (Row s l) = s ++ " & " ++ intercalate "&" (map show l)


-- tableMax = 10
 
formatIteration :: (String,Bool,String,String) -> IterationResult lat -> [Cell lat]
formatIteration (name,flip,opensubscript,closesubscript) (opened, closed)
         = concat $ zipWith (two flip) (format opensubscript opened) (format closesubscript closed)
 where sortMap = sortBy (\x y -> compare (fst x) (fst y)) . M.toAscList
       format s = map (mkCell s) . sortMap
       mkCell s (l,r) = Cell (name++"_{" ++ s ++ "} & \\hspace{-10pt} (" ++ show l ++ ")") r
       two True  a b = [b,a]
       two False a b = [a,b]
 

 
allEqual [] = True
allEqual (x:xs) = all (== x) xs
-- 

-- 
--
--
