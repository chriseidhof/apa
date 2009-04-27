{-# LANGUAGE FlexibleInstances #-}
module Test where

import Control.Applicative
import Analysis
import Label
import Finals
import Types
import Data.Maybe (fromJust)
import Data.List (intercalate)
import qualified Data.Map as M
import BrownPLT.JavaScript.Parser (parseScriptFromString) -- testing
import DataFlowAnalysis.Analysis
import Control.Monad (ap)


test = mapM_ testCase cases

cases :: [(String, String, M.Map Label Lattice -> Err Bool)]
cases = [ ("Simple numbers",           "x = 5",             at 2 ("x" `hasType` numeral))
        , ("Strings",                  "x = 'test'",        at 2 ("x" `hasType` string))
        , deepObjectAssignment
        , objectReferences
        ]
        -- TODO: operator tests

deepObjectAssignment = ("Deep Object Assignment"
                       , "x = {}; x.name = 'chris'; x.test = {age: 12}; x.test.age = '13 yrs'; y = x.name;"
                       , at 44 (     "x" `hasField` ("name", string) 
                                 &&& "y" `hasType` string
                               )
                       )

objectReferences = ("Simple object references"
                   , "x = new Object()"
                   , at 24 (const (return False))
                   )


testCase (name, prog, cond) = case parseScriptFromString "" (prog ++ ";;") of
            Left e  -> error $ "Parsing failed for case " ++ prog
            Right x -> case (cond $ snd $ last $ analyze ana $ label x) of
                            Left err     -> do putStrLn $ "Test failed: " ++ name ++ ": " ++ intercalate ";" err
                                               print (label x)
                            Right False  -> do putStrLn $ "Test failed: " ++ name
                                               print (label x)
                            Right True   -> putStrLn $ "OK: " ++ name

at :: Label -> (Lattice -> Err Bool) -> M.Map Label Lattice -> Err Bool
at x f mp = (fromJust' ("Program point " ++ show x ++ " doesn't exist") $ M.lookup x mp)
        >>= \x -> f x

infixr 3 &&&
(&&&) :: (Lattice -> Err Bool) -> (Lattice -> Err Bool) -> (Lattice -> Err Bool)
(l &&& r) lat = (&&) <$> l lat <*> r lat

hasType :: String -> JsType -> Lattice -> Err Bool
hasType name typ lat = (== [typ]) <$> (fromJust' ("Variable '" ++ name ++ "' doesn't exist") $ M.lookup name $ types lat)

type Err a = Either [String] a

hasField :: [Char] -> (String, JsType) -> Lattice -> Err Bool
hasField name (prop,typ) lat = undefined -- case M.lookup name lat of
                                    -- Nothing -> err $ "No such variable in (hasField) scope : " ++ name
                                    -- Just [Object _ _ props] -> case M.lookup prop props of
                                    --                Nothing -> err $ "No such field: " ++ name
                                    --                Just t  -> Right (t == [typ])

fromJust' _ (Just x) = Right x
fromJust' e Nothing  = Left [e]

err x = Left [x]

-- Monad instances

instance Monad (Either [String]) where
  return = Right
  (Left a)  >>= b        = Left a
  (Right a) >>= b        = b a

instance Applicative (Either [String]) where
  pure = return
  (Left l)  <*> (Left r) = Left (l ++ r)
  (Left l)  <*> _        = Left l
  _         <*> (Left l) = Left l
  (Right x) <*> Right y  = Right (x y)

