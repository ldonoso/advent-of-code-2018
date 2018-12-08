module Utils where

import Data.List
import Data.Ord

getMostFrequent :: (Ord a) => [a] -> a
getMostFrequent = head . maximumBy (comparing length) . group . sort

getTimesMostFrequent :: (Ord a) => [a] -> (a, Int)
getTimesMostFrequent xs =
    let 
        x = head . sortBy (flip $ comparing length) . group . sort $ xs
    in (head x, length x)
