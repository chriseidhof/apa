module WhileTypes where


type Label     = Int
newtype Variable  = V{unV::String}
     deriving (Eq,Ord)

instance Show Variable where
   show = unV

type FlowGraph       = [(Label, Label)]

