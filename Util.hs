module Util where

argcmp :: (Ord b) => (b -> b -> Bool) -> (a -> b) -> [a] -> a
argcmp _   _ [] = error "argmin passed empty list"
argcmp _   _ (n:[]) = n
argcmp cmp f (n:ns) = if f n `cmp` f s then n else s
  where s = argcmp cmp f ns

argmin :: (Ord b) => (a -> b) -> [a] -> a
argmin = argcmp (<)

argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax = argcmp (>)
