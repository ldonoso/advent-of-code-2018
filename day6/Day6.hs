module Day6 where

import System.IO
import Text.Trifecta
import Text.Pretty.Simple (pPrint)
import Data.List
import Data.Ord
import Data.List.Unique (sortUniq)
import Data.Maybe
import Utils (getTimesMostFrequent)

data Point = Point { col :: Int, row :: Int } deriving (Show, Eq, Ord)

pointParser :: Parser Point
pointParser = do
    col <- fromInteger <$> integer
    string ", "
    row <- fromInteger <$> integer
    return $ Point col row

parseInput :: String -> Result [Point]
parseInput = parseString (some $ token $ pointParser) mempty

getDimensions :: [Point] -> (Point, Point)
getDimensions points =
    let
        left_col = minimum $ fmap col points
        top_row = minimum $ fmap row points
        right_col = maximum $ fmap col points
        bot_row = maximum $ fmap row points
    in (Point left_col top_row, Point right_col bot_row)

getPointsInArea :: Point -> Point -> [Point]
getPointsInArea p1 p2 = [(Point c r) | c <- [col p1 .. col p2], r <- [row p1 .. row p2]]

getDistance :: Point -> Point -> Int
getDistance p1 p2 = (abs $ row p1 - row p2) + (abs $ col p1 - col p2)

getDistances :: [Point] -> Point -> [(Point, Int)]
getDistances points p =
    let
        distances = zip points (fmap (getDistance p) points)
    in
        sortBy (comparing snd) distances

getTotalDistance :: [Point] -> Point -> Int
getTotalDistance points p =
    sum $ fmap snd $ getDistances points p

getNearest :: [(Point, Int)] -> Maybe Point
getNearest [] = Nothing
getNearest ((p, d) : []) = Just p
getNearest ((p1, d1) : (p2, d2) : xs) =
    if d1 == d2 then Nothing else Just p1

getNearest' :: [Point] -> Point -> Maybe Point
getNearest' points p = getNearest $ getDistances points p

isInPerifery :: Point -> Point -> Point -> Bool
isInPerifery top_p bottom_p p =
    row p <= row top_p ||
    row p >= row bottom_p ||
    col p <= col top_p ||
    col p >= col bottom_p


solution1 :: [Point] -> (Point, Int)
solution1 points =
    let
        (top_p, bottom_p) = getDimensions points
        points_area = getPointsInArea top_p bottom_p

        points_area' :: [(Point, Point)]
        points_area' = 
            fmap (\(p, Just point_area) -> (p, point_area)) $
            filter (\(p, point_area) -> isJust point_area) $
            zip points_area (fmap (getNearest' points) points_area)

        points_discard =
            sortUniq $ fmap snd $
            filter (\(p, point_area) -> isInPerifery top_p bottom_p p) points_area'

        points_area'' =
            filter
                (\(p, point_area) -> not $ elem point_area points_discard)
                points_area'
    in 
        getTimesMostFrequent . fmap snd $ points_area''


solution2 :: [Point] -> Int
solution2 points =
    let
        (top_p, bottom_p) = getDimensions points
        points_area = getPointsInArea top_p bottom_p

        distances = fmap (getTotalDistance points) points_area

        distances' = filter (< 10000) distances
    in 
        length distances'


main :: String -> IO ()
main filename = do
    inh <- openFile filename ReadMode
    inpStr <- hGetContents inh

    case (parseInput inpStr) of
        Failure e -> print e

        Success points -> do
            putStr "First solution: "
            print $ solution1 points

            putStr "Snd solution: "
            print $ solution2 points

    hClose inh

