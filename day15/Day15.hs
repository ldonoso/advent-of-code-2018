module Day15 where

import System.IO as IO
import Data.Array

type Map = Array (Int, Int) Char

type Elem = Wall | Empty | Unit

readProblemFile :: String -> IO Map
readProblemFile filename = do
    ls <- lines <$> readFile filename
    let nRows = length ls
    let nCols = length $ ls !! 0
    let ls' =  [((c, r), (ls !! r) !! c) | c <- [0 .. nCols - 1], r <- [0 .. nRows - 1]]

    return $ array ((0, 0), (nCols - 1, nRows - 1)) ls'

printMap :: Map -> IO ()
printMap m = mapM_ print ls
    where
        ((c1, r1), (c2, r2)) = bounds m
        ls = [[m ! (c, r) | c <- [c1..c2]] | r <- [r1..r2]]

main :: String -> IO ()
main filename = do
    m <- readProblemFile filename
    printMap m
