module Day3 where

import System.Environment
import System.IO
import Text.Trifecta
import Data.Foldable (foldl')
import Data.Monoid
import Data.Set (Set, fromList, member)
import Data.List (find, groupBy, sort, intersect)
import Data.List.Split (chunksOf)

data Point = Point { row :: Int, column :: Int } deriving (Show, Eq, Ord)

data Square =
    Square { id :: Maybe Int, point :: Point, width :: Int, height :: Int}
    deriving (Show)


squareParser :: Parser Square
squareParser = do
    char '#'
    id <- fromInteger <$> integer
    token $ char '@'
    column <- fromInteger <$> integer
    char ','
    row <- fromInteger <$> integer
    token $ char ':'
    width <- fromInteger <$> integer
    char 'x'
    height <- fromInteger <$> integer
    return $ Square (Just id) (Point row column) width height 


parseInput :: String -> Result [Square]
parseInput = parseString (some $ token $ squareParser) mempty

toPoints :: Square -> [Point]
toPoints (Square _ (Point row column) width height) = [
        Point row column |
        row <- [row .. row + height - 1],
        column <- [column .. column + width - 1]]

anyElem :: (Ord a) => Set a -> [a] -> Bool
anyElem set = not . any (\x -> member x set)

main :: String -> IO ()
main filename = do
    inh <- openFile filename ReadMode
    inpStr <- hGetContents inh

    case (parseInput inpStr) of
        Failure e -> error "Invalid input"

        Success squares -> do
            putStr "Number of conflicting tiles: "
            print . length $ points_colliding

            putStr "Exclusive tail: "
            print $ fst <$>
                find (\x -> anyElem (fromList points_colliding) (snd x))
                (zip squares points)

            where
                points = fmap toPoints $ squares
                points_colliding =
                    fmap head . filter (\x -> length x >= 2) . groupBy (==) . sort . concat $ points

    hClose inh

