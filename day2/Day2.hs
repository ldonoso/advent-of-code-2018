module Day2 where

import System.IO
import Text.Trifecta
import Data.Foldable (foldl')
import Data.Monoid
import qualified Data.Set as Set
import Data.List (sort)

parseInput :: String -> Result [String]
parseInput = parseString (some $ token $ some letter) mempty

partitionate :: (Ord a) => [a] -> [[a]]
partitionate xs =
    foldl' go [] (sort xs)
    where
        go [] x = [[x]]
        go l@((a2:a2s):as) x
            | a2 == x = (x:a2:a2s) : as
            | otherwise = [x] : l

hasAnyOfLength :: Int -> [[a]] -> Bool
hasAnyOfLength n xs = any (\x -> length x == n) xs

countIfPred :: (a -> Bool) -> [a] -> Integer
countIfPred f = foldl' (\a x -> if f x then a + 1 else a) 0

process :: [[String]] -> Integer
process xs = twices * thrices
    where
        twices = countIfPred (hasAnyOfLength 2) xs
        thrices = countIfPred (hasAnyOfLength 3) xs

expand :: [a] -> [[a]]
expand = go []
    where
        go _ [] = []
        go lhs (x:xs) = (lhs ++ xs) : (go (lhs ++ [x]) xs)

detectTwins :: [[String]] -> Maybe String
detectTwins xs = go Set.empty 0 xs
    where
        go set pos [] = Nothing
        go set pos ([] : xs) = go set 0 xs
        go set pos ((x:xs2) : xs)
            | Set.member (pos, x) set = Just x
            | otherwise = go (Set.insert (pos, x) set) (pos + 1) (xs2 : xs)
    

main :: String -> IO ()
main filename = do
    inh <- openFile filename ReadMode
    inpStr <- hGetContents inh

    case (parseInput inpStr) of
        Failure e -> undefined

        Success input -> do
            putStr "CRC: "
            print . process. (fmap partitionate) $ input

            putStr "Box code: "
            print . detectTwins . (fmap expand) $ input

    hClose inh

