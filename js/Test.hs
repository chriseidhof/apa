{-# LANGUAGE FlexibleInstances #-}
module Test where

import Control.Applicative
import Analysis hiding (fromJust')
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
        , simpleObject
        , objectAssignment
        , deepObjectAssignment
        ]

simpleObject = ( "Simple object"
               , "x = new Object()"
               ,  at 8 (    isReference "x" 5
                        &&& 5 `references` Object Nothing M.empty Nothing
                       )
               )

objectAssignment = ( "Simple object"
                   , "x = new Object(); x.name = 'test'"
                   ,  at 16 (    "x" `isReference` 5
                             &&& 5   `hasField` ("name", string)
                            )
                   )
deepObjectAssignment = ( "Deep object assignment"
                   , "x = new Object(); x.sub = new Object(); x.sub.name = 'test'"
                   ,  at 28 ( "x" `isReference` 5
                          &&& 5   `hasField` ("sub", ref 15)
                          &&& 15  `hasField` ("name", string)
                           )
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

isReference :: String -> Int -> Lattice -> Err Bool
isReference s i = hasType s (Reference $ Ref i)

references :: Int -> Object -> Lattice -> Err Bool
references x o lat = (== o) <$> (fromJust' ("Reference '" ++ show x ++ "' doesn't exist") $ M.lookup (Ref x) $ refs lat)

hasField :: Int -> (String, JsType) -> Lattice -> Err Bool
hasField addr (prop,typ) lat = case M.lookup (Ref addr) (refs lat) of
                                    Nothing -> err $ "No such reference in (hasField) scope : " ++ show addr
                                    Just (Object _ props _ ) -> case M.lookup prop props of
                                                    Nothing -> err $ "No such field: " ++ prop
                                                    Just t  -> Right (t == [typ])


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

fromJust' _ (Just x) = Right x
fromJust' e Nothing  = Left [e]
