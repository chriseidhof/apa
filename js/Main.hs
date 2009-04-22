module Main where

import BrownPLT.JavaScript.Parser (parseString)
import BrownPLT.JavaScript.Syntax
import BrownPLT.JavaScript.Environment
import System.Environment (getArgs)

main = do
  [fileName] <- getArgs
  contents <- readFile fileName
  let x = parseString contents
  -- mapM_ print $ map (fmap fst3) ann
  -- print env1
  -- print env2
  -- print i
  print x

fst3 (a,_,_) = a
