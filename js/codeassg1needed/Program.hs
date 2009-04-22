module Program where

import MonotoneFramework
import Aux

class Program prg where
   labels :: prg -> [Label]
   init   :: prg -> Label
   final  :: prg -> [Label]
   flow   :: prg -> FlowGraph

flowR :: (Program prg) => prg -> FlowGraph
flowR = map swap . flow


