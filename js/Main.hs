module Main where

import WebBits.JavaScript.Parser (parseString)
import WebBits.JavaScript.Syntax
import WebBits.JavaScript.Environment
import System.Environment (getArgs)

main = do
  [fileName] <- getArgs
  contents <- readFile fileName
  let (ann, env1, env2, i) = staticEnvironment $ parseString contents
  mapM_ print $ map (fmap fst3) ann
  print env1
  print env2
  print i

fst3 (a,_,_) = a
