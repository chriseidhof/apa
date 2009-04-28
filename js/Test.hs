{-# LANGUAGE FlexibleInstances #-}
module Test where

import Control.Applicative
import Analysis hiding (fromJust')
import Label
import Finals
import Types
import Data.Maybe (fromJust)
import Data.List (intercalate, sort)
import qualified Data.Map as M
import BrownPLT.JavaScript.Parser (parseScriptFromString) -- testing
import DataFlowAnalysis.Analysis
import Control.Monad (ap)


test = mapM_ testCase cases

cases :: [(String, String, M.Map Label Lattice -> Err Bool)]
cases = [ ("Simple numbers",           "x = 5",             at 12 ("x" `hasType` numeral))
        , ("Strings",                  "x = 'test'",        at 12 ("x" `hasType` string))
        , merging
        , simpleObject
        , objectAssignment
        , deepObjectAssignment
        , loopObjectAssignment
        , functions
        , prototyping
        , prototypeAccess
        ]

simpleObject = ( "Simple object"
               , "x = new Object()"
               ,  at 18 (    isReference "x" 15
                        &&& 15 `references` Object Nothing M.empty Nothing
                       )
               )

objectAssignment = ( "Simple object"
                   , "x = new Object(); x.name = 'test'"
                   ,  at 26 (    "x" `isReference` 15
                             &&& 15   `hasField` ("name", string)
                            )
                   )
deepObjectAssignment = ( "Deep object assignment"
                   , "x = new Object(); x.sub = new Object(); x.sub.name = 'test'"
                   ,  at 38 ( "x" `isReference` 15
                          &&& 15   `hasField` ("sub", ref 25)
                          &&& 25  `hasField` ("name", string)
                           )
                   )

loopObjectAssignment = ( "Loop object assignment"
                   , "x = new Object(); x.sub = new Object(); x.sub.name = x"
                   ,  at 39 ( "x" `isReference` 15
                          &&& 15   `hasField` ("sub", ref 25)
                          &&& 25  `hasField` ("name", ref 15)
                           )
                   )

merging = ( "Merging in if/else"
          , "if(y) { x = 13;} else {x = 'hi';}"
          , at 30 ( "x" `hasTypes` [string, numeral])
          )

functions   = ( "Functions"
              , "MyClass = function(){}"
              , at 17 (    "MyClass" `isReference` 15
                      &&& 15         `hasValueType` Function
                      &&& 15         `hasPrototype` refBuiltInFunction
                      )
              )

prototyping = ( "Prototyping"
              , "MyClass = function(){}; MyClass.prototype.foo = 'hi'; x = new MyClass()"
              , at 35 (    "MyClass" `isReference` 15
                       &&& "x"       `isReference` 32
                       &&& 32        `hasPrototype` (Ref $ -15)
                       &&& (-15)     `hasField`     ("foo", string)
                      )
              )

prototypeAccess = ( "Prototype access"
                  , "MyClass = function(){}; MyClass.prototype.foo = 'hi'; x = new MyClass(); y = x.foo"
                  , at 44 (    "MyClass" `isReference` 15
                           &&& "x"       `isReference` 32
                           &&& 32        `hasPrototype` (Ref $ -15)
                           &&& (-15)     `hasField`     ("foo", string)
                           &&& "y"       `hasType`      string
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
hasType name typ lat = hasTypes name [typ] lat

hasTypes :: String -> [JsType] -> Lattice -> Err Bool
hasTypes name typ lat = (eq typ) <$> (fromJust' ("Variable '" ++ name ++ "' doesn't exist") $ M.lookup name $ types lat)
 where eq a b | sort a == sort b = True
              | otherwise = error $ "/=" ++ show (a, b)

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

hasValueType :: Int -> PrimitiveType -> Lattice -> Err Bool
hasValueType addr typ lat = case M.lookup (Ref addr) (refs lat) of
                                    Nothing -> err $ "No such reference in (hasValueType) scope : " ++ show addr
                                    Just (Object t _ _ ) -> Right (t == Just typ)

hasPrototype :: Int -> Ref -> Lattice -> Err Bool
hasPrototype addr ref lat = case M.lookup (Ref addr) (refs lat) of
                                    Nothing -> err $ "No such reference in (hasPrototype) scope : " ++ show addr
                                    Just (Object _ _ prot) -> Right (prot == Just ref)


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
