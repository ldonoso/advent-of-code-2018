module Day10 where

import System.IO
import Text.Trifecta
import qualified Data.Set as Set
import Data.List
import Data.List.Split (chunksOf)
import Data.Ord

data Point = Point { pX :: Int, pY :: Int } deriving (Show, Eq, Ord)
data Vel = Vel { vX :: Int, vY :: Int } deriving (Show, Eq, Ord)

tupleParser :: Num a => Parser (a, a)
tupleParser = do
    token $ char '<'
    x <- fromInteger <$> integer
    token $ char ','
    y <- fromInteger <$> integer
    return $ (x, y)
    token $ char '>'
    return (x, y)

unitParser :: Parser (Point, Vel)
unitParser = do
    string "position="
    (px, py) <- tupleParser
    string "velocity="
    (vx, vy) <- tupleParser
    return ((Point px py), (Vel vx vy))

parseInput :: String -> Result [(Point, Vel)]
parseInput = parseString (some $ token $ unitParser) mempty

getDimensions :: [Point] -> (Point, Point)
getDimensions points =
    let
        x1 = minimum $ fmap pX points
        y1 = minimum $ fmap pY points
        x2 = maximum $ fmap pX points
        y2 = maximum $ fmap pY points
    in (Point x1 y1, Point x2 y2)


getArea :: [Point] -> Integer
getArea points = toInteger (getWidth p1 p2) * toInteger (getHeight p1 p2)
    where (p1, p2) = getDimensions points


getWidth :: Point -> Point -> Int
getWidth p1 p2 = pX p2 - pX p1 + 1

getHeight :: Point -> Point -> Int
getHeight p1 p2 = pY p2 - pY p1 + 1


getPoints :: Point -> Point -> [Point]
getPoints p1 p2 = [Point x y | y <- [pY p1 .. pY p2], x <- [pX p1 .. pX p2]]

getPicture :: [Point] -> [(Point, Char)]
getPicture points = do
    let (p1, p2) = getDimensions points
    let pointsSet = Set.fromList points
    let pointsAll = getPoints p1 p2
    let pointsRep = fmap (\x -> if Set.member x pointsSet then '#' else '.') pointsAll
    zip pointsAll pointsRep

printPicture :: Int -> [Char] -> IO [()]
printPicture width xs = sequenceA $ fmap putStrLn (chunksOf width xs)

runStep :: [Vel] -> [Point] -> [Point]
runStep vels points =
    fmap (\(v, p) -> Point (pX p + vX v) (pY p + vY v)) $
    zip vels points

main :: String -> IO ()
main filename = do
    inh <- openFile filename ReadMode
    inpStr <- hGetContents inh

    case (parseInput inpStr) of
        Failure e -> error "Invalid input"

        Success d -> do
            let vels = snd <$> d
            let points = fst <$> d
            let pointsList = iterate (runStep vels) points
            let (it, points') = minimumBy (comparing (getArea . snd)) . zip [0..] . take 100000 $ pointsList

            putStrLn "Fst solution:"
            printPicture (uncurry getWidth $ getDimensions $ points') (snd <$> getPicture points')
            
            print "Snd solution:"
            print it
            

    hClose inh

