module DataFlowAnalysis.Aux where
import Control.Arrow

singl :: a -> [a]
singl = return

swap = snd &&& fst

(f `split` g) x = (f x,g x)
usplit = uncurry split


