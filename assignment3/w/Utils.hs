module Utils where

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

f <$> x = fmap (const f) x
t @@  a = fmap (const a) t

lookup' x = maybe (error $ "No such variable: " ++ show x) id . lookup x

gamma @-> x = x:gamma
