module Day10 where

import System.IO
import Text.Trifecta
import Text.Pretty.Simple (pPrint)

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

main :: String -> IO ()
main filename = do
    inh <- openFile filename ReadMode
    inpStr <- hGetContents inh

    case (parseInput inpStr) of
        Failure e -> error "Invalid input"

        Success d -> do
            putStr "Fst solution: "
            pPrint $ d

    hClose inh

