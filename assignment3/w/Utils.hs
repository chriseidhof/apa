module Utils where

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

f <$> x = fmap (const f) x
t @@  a = fmap (const a) t
