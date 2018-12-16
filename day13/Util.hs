module Util where

cyclicSucc :: (Enum a, Bounded a, Eq a) => a -> a
cyclicSucc x | x == maxBound = minBound
             | otherwise     = succ x
