module Day14 where

import qualified Data.Sequence as S
import Test.Hspec
import Data.Maybe
import Debug.Trace
import Data.Int

data Recipes = Recipes { rs :: S.Seq Int8, p1 :: Int, p2 :: Int} deriving (Show)

newRecipes :: Recipes -> Recipes
newRecipes (Recipes rs p1 p2) = Recipes rs' (updatePos p1) (updatePos p2)
    where
        rs' =
            if p' < 10 then rs S.|> p' else rs S.|> div p' 10 S.|> p' `mod` 10
            where p' = S.index rs p1 + S.index rs p2
        updatePos pos = (pos + 1 + (fromIntegral $ S.index rs pos)) `mod` (S.length rs')

recipesList :: [Recipes]
recipesList = iterate newRecipes (Recipes (S.fromList [3, 7]) 0 1)

solution1 :: Int -> S.Seq Int8
solution1 nRecipes = 
    S.drop nRecipes $
    rs . head $
    dropWhile ((< nRecipes + 10) . S.length . rs) $
    recipesList

solution2 :: S.Seq Int8 -> Maybe Int
solution2 n = 
    head $
    dropWhile isNothing $
    fmap (numberOfCoincidences n) $
    fmap rs $ recipesList

numberOfCoincidences :: S.Seq Int8 -> S.Seq Int8 -> Maybe Int
numberOfCoincidences n h =
    case matches10 n h of
        Just x -> Just x
        Nothing ->
            case matches11 n h of 
                Just x -> Just x
                Nothing -> Nothing

matches10 :: S.Seq Int8 -> S.Seq Int8 -> Maybe Int
matches10 n h =
    if hR == n then Just (length hL) else Nothing
    where
        (hL, hR) = S.splitAt (S.length h - S.length n) h

matches11 :: S.Seq Int8 -> S.Seq Int8 -> Maybe Int
matches11 n h =
    if S.take lengthN hR == n then Just (length hL) else Nothing
    where
        lengthN = S.length n
        (hL, hR) = S.splitAt (S.length h - lengthN - 1) h


test :: IO ()
test = hspec $ do
    describe "Part 2" $ do
        it "Part 2" $ do
            solution2 (S.fromList [5, 1, 5, 8, 9]) `shouldBe` Just 9
            solution2 (S.fromList [0, 1, 2, 4, 5]) `shouldBe` Just 5
            solution2 (S.fromList [9, 2, 5, 1, 0]) `shouldBe` Just 18
            solution2 (S.fromList [5, 9, 4, 1, 4]) `shouldBe` Just 2018

main :: IO ()
main = do
    let recipesList = iterate newRecipes (Recipes (S.fromList [3, 7]) 0 1)

    putStr "Fst solution: " 
    print $ solution1 260321

    putStr "Snd solution: " 
    print $ solution2 (S.fromList [2, 6, 0, 3, 2, 1])
