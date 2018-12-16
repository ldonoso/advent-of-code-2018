module Day13 where

import System.IO as IO
import Data.Array
import Debug.Trace
import Data.Tuple (swap)
import Data.List (sort, elemIndex, delete)
import Util (cyclicSucc)

dirs :: [Char]
dirs = "><^v"

isHoriz :: Char -> Bool
isHoriz c = elem c "<>"

data NextInters = L | S | R deriving (Show, Eq, Bounded, Enum)

type Pos = (Int, Int)
data Veh = Veh {pos :: Pos, dir :: Char, nextInters :: NextInters} deriving (Show, Eq)

instance Ord Veh where
    compare v1 v2 = compare (swap $ pos v1) (swap $ pos v2)

type Map = Array (Int, Int) Char

movVeh :: Char -> Veh -> Veh
movVeh mapElem v@(Veh (c, r) dir nextInters) = 
    case mapElem of
        '|' ->
            case dir of
                '^' -> Veh (c, r - 1) dir nextInters
                'v' -> Veh (c, r + 1) dir nextInters
        '-' ->
            case dir of
                '>' -> Veh (c + 1, r) dir nextInters
                '<' -> Veh (c - 1, r) dir nextInters
        '/' ->
            case dir of
                '^' -> Veh (c + 1, r) '>' nextInters
                'v' -> Veh (c - 1, r) '<' nextInters
                '<' -> Veh (c, r + 1) 'v' nextInters
                '>' -> Veh (c, r - 1) '^' nextInters
        '\\' ->
            case dir of
                '^' -> Veh (c - 1, r) '<' nextInters
                'v' -> Veh (c + 1, r) '>' nextInters
                '<' -> Veh (c, r - 1) '^' nextInters
                '>' -> Veh (c, r + 1) 'v' nextInters

        '+' -> movVeh mapElem' (Veh (c, r) dir (cyclicSucc nextInters))
            where
                mapElem' = case nextInters of
                    L -> if isHoriz dir then '/' else '\\'
                    S -> if isHoriz dir then '-' else '|'
                    R -> if isHoriz dir then '\\' else '/'

readProblemFile :: String -> IO Map
readProblemFile filename = do
    ls <- lines <$> readFile filename
    let nRows = length ls
    let nCols = length $ ls !! 0
    let ls' =  [((c, r), (ls !! r) !! c) | c <- [0 .. nCols - 1], r <- [0 .. nRows - 1]]

    return $ array ((0, 0), (nCols - 1, nRows - 1)) ls'

getVehs :: Map -> [Veh]
getVehs m =
    fmap (\i -> Veh i (m ! i) L) $
    filter (\i -> (m ! i) `elem` dirs) $
    indices m

turnVehIntoElem :: Char -> Char
turnVehIntoElem c 
    | elem c "<>" = '-'
    | elem c "^v" = '|'
    | otherwise = c

collides :: [Veh] -> Veh -> Bool
collides vs v = case elemIndex (pos v) $ fmap pos vs of
    Nothing -> False
    otherwise -> True


movVehs :: Map -> [Veh] -> [Veh] -> Either Pos [Veh]
movVehs m [] vsMoved = Right vsMoved
movVehs m (v:vs) vsMoved =
    if collides vsMoved v' || collides vs v' then
        Left (pos v')
    else
        movVehs m vs (v':vsMoved)
    where
        v' = movVeh (m ! pos v) v


solution1 :: Map -> [Veh] -> Pos
solution1 m vs =
    case movVehs m (sort vs) [] of
        Left pos -> pos
        Right vs' -> solution1 m (sort vs')


movVehs' :: Map -> [Veh] -> [Veh] -> [Veh]
movVehs' m [] vsMoved = vsMoved
movVehs' m (v:vs) vsMoved =
    if collides vsMoved v' || collides vs v' then
        movVehs' m (removeInPos vs) (removeInPos vsMoved)
    else
        movVehs' m vs (v':vsMoved)
    where
        v' = movVeh (m ! pos v) v
        removeInPos = filter ((/= pos v') . pos)


solution2 :: Map -> [Veh] -> Pos
solution2 m vs =
    case movVehs' m (sort vs) [] of
        (v:[]) -> pos v
        vs -> solution2 m (sort vs)


main :: String -> IO ()
main filename = do
    map <- readProblemFile filename
    let vehs = getVehs map
    let map' = turnVehIntoElem <$> map
    print $ solution1 map' vehs
    print $ solution2 map' vehs
