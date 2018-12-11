module Day9 where

import System.IO
import Text.Trifecta
import Data.List
import Data.Ord (comparing)
import Text.Pretty.Simple
import Test.Hspec


parserMarbles :: Parser (Int, Int)
parserMarbles = do
    players <- fromInteger <$> integer
    string "players; last marble is worth "
    points <- fromInteger <$> integer
    string "points"
    return (players, points)


data Game = Game { marbles :: [Int], current :: Int, scores :: [(Int, Int)]} deriving (Show)


getPos :: Int -> Int -> [Int] -> Int
getPos current offset xs = (current + offset) `mod` (length $ xs)


addMarble :: Game -> Int -> Int -> Game
addMarble (Game marbles current scores) player marble
    | marble `mod` 23 == 0 =
        let 
            pos = getPos current (-7) marbles
            (l, r) = splitAt pos marbles
            (marbles', marbleRemoved) = remove l r
            scores' = reduceScores $ (player, marble + marbleRemoved) : scores
        in 
            Game marbles' pos scores'

    | otherwise = 
        let 
            pos = getPos current 2 marbles
            (l, r) = splitAt pos marbles
        in 
            Game (l ++ (marble : r)) pos scores

    where
        remove l (r:rs) = (l ++ rs, r)


getGames :: Game -> Int -> Int -> [Game]
getGames game players marbles = go game (zip (cycle [1 .. players]) [1 .. marbles]) 
    where
        go game [] = []
        go game ((player, marble):xs) =
            let game' = addMarble game player marble
            in game' : go game' xs

getLastGame :: Game -> Int -> Int -> Game
getLastGame game players marbles = go game (zip (cycle [1 .. players]) [1 .. marbles]) 
    where
        go game [] = game
        go game ((player, marble):xs) = go (addMarble game player marble) xs

reduceScores :: [(Int, Int)] -> [(Int, Int)]
reduceScores =
    fmap (\x -> (fst . head $ x, getTotal x))
    . groupBy (\l r -> fst l == fst r)
    . sortBy (comparing fst)
    where getTotal = sum . fmap snd


getWinner :: [(Int, Int)] -> Int
getWinner = maximum . fmap snd . reduceScores

play :: Int -> Int -> Int
play players marbles = 
    let game = getLastGame (Game [0] 0 []) players marbles
    in getWinner $ scores game

        
main :: String -> IO ()
main filename = do
    inh <- openFile filename ReadMode
    inpStr <- hGetContents inh
    
    let x = parseString parserMarbles mempty inpStr

    case x of
        Failure e -> print e

        Success (players, marbles) -> do
            putStr "Fst solution: "
            print $ play players marbles

    hClose inh


printAdvance :: (Show a) => Int -> [a] -> IO ()
printAdvance ctr [] = putStrLn $ "Processed " ++ show ctr
printAdvance ctr xs = do
    let step = 5000
    let (l, r) = splitAt step xs
    let ctr' = ctr + length l
    
    print $ last l
    putStrLn $ "Processed " ++ show ctr'
    printAdvance ctr' r


test :: IO ()
test = hspec $ do
    it "Day9 tests" $ do
        play 9 25 `shouldBe` 32
        play 10 1618 `shouldBe` 8317
        -- play 13 7999 `shouldBe` 146373
        -- play 466 71436 `shouldBe`
