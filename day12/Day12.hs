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

trace' a = trace (show a) a

type Vals = Seq.Seq Char
data State = State Vals Int deriving (Show)

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

tokens :: Vals
tokens = Seq.fromList "..."


extend :: State -> State
extend = extendRight . extendLeft where
    extendLeft :: State -> State
    extendLeft s@(State vals pos)
        | (slice 0 3 vals) /= tokens = State (tokens Seq.>< vals) (pos + 3)
        | otherwise = s
        
    extendRight :: State -> State
    extendRight s@(State vals pos)
        | (slice (subtract 3 (length vals)) 3 vals) /= tokens = State (vals Seq.>< tokens) pos
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
play rules s = iterate (doEvolution rules . extend) s

getResult :: State -> Int
getResult (State vals pos) =
    sum $ fmap fst $ filter (\x -> snd x == '#') $ zip [negate pos..] (toList vals)

solution1 :: Int -> State -> RulesMap -> Int
solution1 nIter state rules =
    let states = take nIter $ play rules state
    in getResult $ last states

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
    describe "Solution 1" $ do
        it "Test" $ do
            (state, rules) <- readProblemFile "input_test.txt"
            solution1 21 state rules `shouldBe` 325

        it "Real problem" $ do
            (state, rules) <- readProblemFile "input.txt"
            solution1 21 state rules `shouldBe` 3410


main :: Int -> String -> IO ()
main nIter filename = do
    (state, rules) <- readProblemFile filename
    print $ solution1 nIter state rules
