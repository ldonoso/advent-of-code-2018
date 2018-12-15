module Day12 where

import System.IO as IO
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Test.Hspec
import Text.Trifecta
import Text.Pretty.Simple (pPrint)
import Data.Maybe
import Data.Foldable (toList)
import Debug.Trace (trace)
import Data.Foldable (foldl')

trace' a = trace (show a) a

type Vals = Seq.Seq Char
data State = State { vals :: Vals, pos :: Integer } deriving (Show)

valParser :: Parser Char
valParser = oneOf "#."

ruleParser :: Parser ([Char], Char)
ruleParser = do
    k <- some valParser <* string " => "
    v <- valParser <* newline
    return (k, v)

type RulesMap = Map.Map [Char] Char
    
dataParser :: Parser (State, RulesMap)
dataParser = do
    vals <- string "initial state: " *> (some valParser) <* some newline
    rules <- some ruleParser
    return (State (Seq.fromList vals) 0, Map.fromList rules)


slice :: Int -> Int -> Seq.Seq a -> Seq.Seq a
slice pos width s = Seq.take width $ Seq.drop pos s

tokens3 :: Vals
tokens3 = Seq.fromList "..."

normalize :: State -> State
normalize = normalizeRight . normalizeLeft where
    normalizeLeft :: State -> State
    normalizeLeft s@(State vals pos)
        | (slice 0 3 vals) /= tokens3 = State (tokens3 Seq.>< vals) (pos - 3)
        | otherwise = s
        
    normalizeRight :: State -> State
    normalizeRight s@(State vals pos)
        | (slice (subtract 3 (length vals)) 3 vals) /= tokens3 = State (vals Seq.>< tokens3) pos
        | otherwise = s

doEvolution :: RulesMap -> State -> State
doEvolution rules (State vals pos) = State vals' pos
    where
        getNew pos 
            | pos - 2 < 0 = '.'
            | pos + 2 >= length vals = '.'
            | otherwise = Map.findWithDefault '.' (toList $ slice (pos - 2) 5 vals) rules

        vals' = Seq.fromList $ getNew <$> [0..length vals]

play :: RulesMap -> State -> [State]
play rules s = iterate (doEvolution rules . normalize) s

getResult :: State -> Integer
getResult (State vals pos) =
    sum $ fmap fst $ filter (\x -> snd x == '#') $ zip [pos..] (toList vals)

solution1 :: Int -> State -> RulesMap -> Integer
solution1 nIter state rules =
    let states = take nIter $ play rules state
    in getResult $ last states

solution1' :: Int -> State -> RulesMap -> Integer
solution1' nIter state rules =
    getResult $
    foldl' (\acc _ -> doEvolution rules . normalize $ acc) state [1..nIter]
    

findRep :: State -> RulesMap -> (Integer, State, State)
findRep state rules = go state 0
    where
        removeEmpty s = Seq.dropWhileL (/= '#') $ Seq.dropWhileL (/= '#') s
        equivalentSeq s1 s2 = removeEmpty s1 == removeEmpty s2
        go s it
            | equivalentSeq (vals s) (vals s') = (it, s, s')
            | otherwise = go (s') (it + 1)
            where s' = doEvolution rules . normalize $ s

normalize' :: State -> State
normalize' (State vals pos) = 
    let (Just pos') = Seq.elemIndexL '#' vals
    in State (Seq.drop pos' vals) (pos + toInteger pos')

solution2 :: State -> RulesMap -> Integer
solution2 state rules =
    let
        (nIter, s1, s2) = findRep state rules
        s1' = normalize' s1
        s2' = normalize' s2
        diff = (pos s2' - pos s1')
        posFinal = pos s1' + ((50 * 10^9) - nIter)
    in getResult (State (vals s1') posFinal)


readProblemFile :: String -> IO (State, RulesMap)
readProblemFile filename = withFile filename ReadMode (\h -> do
    inpStr <- hGetContents h

    case (parseString dataParser mempty inpStr) of
        Failure e -> error "Invalid input"

        Success (state, rules) -> do
            return (state, rules)
    )

test :: IO ()
test = hspec $ do
    describe "Day 12" $ do
        it "Test case" $ do
            (state, rules) <- readProblemFile "input_test.txt"
            solution1 21 state rules `shouldBe` 325

        it "Part 1" $ do
            (state, rules) <- readProblemFile "input.txt"
            solution1 21 state rules `shouldBe` 3410

        it "Part 2" $ do
            (state, rules) <- readProblemFile "input.txt"
            solution2 state rules `shouldBe` 4000000001480


main :: Int -> String -> IO ()
main nIter filename = do
    (state, rules) <- readProblemFile filename
    print $ solution1' nIter state rules
