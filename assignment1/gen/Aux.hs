module Aux where

singl :: a -> [a]
singl = return

(f `split` g) x = (f x,g x)
usplit = uncurry split


