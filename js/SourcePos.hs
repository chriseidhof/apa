{-# LANGUAGE DeriveDataTypeable #-}
module SourcePos where

import Text.ParserCombinators.Parsec (SourcePos)
import Data.Generics

-- TODO: we need to correct this to not throw all information away.
data SourcePosition = SP
 deriving (Data, Typeable, Eq, Show)

transformSourcePos :: a -> SourcePosition
transformSourcePos = const SP
