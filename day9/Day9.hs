module Day9 where

import System.IO
import Text.Trifecta
import Data.List
import Data.Ord (comparing)
import Text.Pretty.Simple
import Test.Hspec
import Utils
import qualified Data.Sequence as DS


parserMarbles :: Parser (Int, Int)
parserMarbles = do
    nPlayers <- fromInteger <$> integer
    string "players; last marble is worth "
    points <- fromInteger <$> integer
    string "points"
    return (nPlayers, points)


data Game = Game { marbles :: DS.Seq Int, scores :: [(Int, Int)]} deriving (Show)


addMarble :: Game -> Int -> Int -> Game
addMarble (Game marbles scores) player marble
    | marble `mod` 23 == 0 =
        let 
            marbles' = rotate (-7) marbles
            scores' = (player, marble + DS.index marbles' 0) : scores
        in 
            Game (DS.drop 1 marbles') scores'

    | otherwise = Game (marble DS.<| rotate 2 marbles) scores


getLastGame :: Game -> [(Int, Int)] -> Game
getLastGame game moves = foldl' (\g (p, m) -> addMarble g p m) game moves


reduceScores :: [(Int, Int)] -> [(Int, Integer)]
reduceScores =
    fmap (\x -> (fst . head $ x, getTotal x))
    . groupBy (\l r -> fst l == fst r)
    . sortBy (comparing fst)
    where
        getTotal :: [(Int, Int)] -> Integer
        getTotal = sum . fmap (toInteger . snd)


getWinner :: [(Int, Int)] -> Integer
getWinner = maximum . fmap snd . reduceScores


play :: Int -> Int -> Integer
play nPlayers marbles = 
    let
        moves = zip (cycle [1 .. nPlayers]) [1 .. marbles]
        game = getLastGame (Game (DS.singleton 0) []) moves
    in getWinner $ scores game


main :: String -> IO ()
main filename = do
    inh <- openFile filename ReadMode
    inpStr <- hGetContents inh
    
    let x = parseString parserMarbles mempty inpStr

    case x of
        Failure e -> print e

        Success (nPlayers, marbles) -> do

            putStr "Fst solution: "
            print $ play nPlayers marbles

            putStr "Snd solution: "
            print $ play nPlayers (marbles * 100)

    hClose inh


test :: IO ()
test = hspec $ do
    it "Day9 tests" $ do
        play 9 25 `shouldBe` 32
        play 10 1618 `shouldBe` 8317
        play 13 7999 `shouldBe` 146373
        play 466 71436 `shouldBe` 382055
