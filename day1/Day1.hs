module Day1 where

import System.IO
import Text.Trifecta
import Data.Foldable (foldl')
import Data.Monoid
import qualified Data.Set as Set

parseInput :: String -> Result [Integer]
parseInput = parseString (many integer) mempty

processInput :: [Integer] -> Integer
processInput = sum

acc :: (Monoid a) => [a] -> [a]
acc = go mempty
    where
        go acc [] = []
        go acc (x:xs) = (acc <> x) : (go (acc <> x) xs)

firstRepeated :: (Ord a) => [a] -> Maybe a
firstRepeated = go Set.empty
    where
        go _ [] = Nothing
        go set (x:xs) 
            | Set.member x set = Just x
            | otherwise = go (Set.insert x set) xs

main :: IO ()
main = do
    inh <- openFile "input.txt" ReadMode
    inpStr <- hGetContents inh

    case (parseInput inpStr) of
        Failure e -> undefined

        Success numbers -> do
            putStr "Total frequency: "
            print . processInput $ numbers

            putStr "First frequency repeated twice: "
            let accumulated = acc . (fmap Sum) . cycle $ numbers
            print . firstRepeated $ accumulated

    hClose inh

