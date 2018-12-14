module Day11 where

import Utils
import qualified Data.Sequence as DS
import Data.List
import Data.Ord
import Test.Hspec
import Data.Foldable (toList)
import Debug.Trace (trace)

getHundred :: Int -> Int
getHundred x = mod (div x 100) 10

getPowerLevel :: Int -> (Int, Int) -> Int
getPowerLevel serial (x, y) =
    subtract 5 $ getHundred $ (* rack_id) $ (+ serial) $ rack_id * y
    where rack_id = (x + 10)

getGrid :: Int -> Int -> Int -> Int -> [(Int, Int)]
getGrid cStart rStart nCols nRows =
    [(c, r) | c <- [cStart .. cStart + nCols - 1], r <- [rStart .. rStart + nRows - 1]]


getPowerGrid :: Int -> Int -> Matrix2D Int
getPowerGrid serial width = Matrix2D width width vals
    where vals =
            DS.fromList $ fmap (getPowerLevel serial) $
            [(c, r) | c <- [1 .. width], r <- [1 .. width]]

getPowerGridSum :: Matrix2D Int -> Int -> Matrix2D Int
getPowerGridSum m@(Matrix2D nCols nRows vals) widthSub = Matrix2D nCols nRows vals'
    where vals' =
            DS.fromList $
            fmap sum $
            fmap (matrix2DSlice m widthSub widthSub) $
            fmap (matrix2DGetCoord m) $
            [0 .. nCols * nRows - 1]

test :: IO ()
test = hspec $ do
    describe "Utilities" $ do
        it "getPowerLevel tests" $ do
            getPowerLevel 8 (3, 5) `shouldBe` 4
            getPowerLevel 57 (122, 79) `shouldBe` (-5)
            getPowerLevel 39 (217, 196) `shouldBe` 0
            getPowerLevel 71 (101, 153) `shouldBe` 4

    describe "solutions" $ do
        it "solution1" $ do
            solution1 9995 300 3 `shouldBe` (33, 45)

        it "solution1'" $ do
            solution1' 9995 300 3 `shouldBe` (33, 45)


getPowerGridMax :: Matrix2D Int -> Int -> ((Int, Int), Int)
getPowerGridMax grid widthSub =
    let
        powerGridSum :: Matrix2D Int
        powerGridSum = getPowerGridSum grid widthSub
        
        (pos, score) = maximumBy (comparing snd) $ zip [0..] (toList $ vector powerGridSum)
        p@(c, r) = matrix2DGetCoord powerGridSum pos
    in ((c+1, r+1), score)
    

getPowerElem :: Matrix2D Int -> Int -> (Int, Int) -> Int
getPowerElem m widthSub p
    | widthSub == 1 = DS.index (vector m) (matrix2DGetIndex m p)
    | otherwise = getPowerElem m (widthSub - 1) p + extra
        where extra = sum $ matrix2DGetCorners m widthSub p


getPowerElems :: Matrix2D Int -> (Int, Int) -> [(Int, Int)]
getPowerElems m p@(c, r) = zip sizes powerLevels
    where
        sizes = [1 .. min (nCols m - c) (nRows m - r)]

        fun acc 1 = [DS.index (vector m) (matrix2DGetIndex m p)]
        fun acc size = head acc + (sum $ matrix2DGetCorners m size p) : acc

        powerLevels = foldl' fun [] sizes

solution2' :: Int -> Int -> ((Int, Int), (Int, Int))
solution2' serial width =
    let
        m = getPowerGrid serial width
        points = fmap (matrix2DGetCoord m) [0 .. nCols m * nRows m - 1]
        results =
            fmap (\p -> maximumBy (comparing snd) $
            getPowerElems m (trace (show p) p)) points
        r' = zip points results

    in maximumBy (comparing (snd . snd)) r'


getPowerGridMax' :: Matrix2D Int -> Int -> [((Int, Int), Int)]
getPowerGridMax' m widthSub =
    fmap (\p -> (p, (getPowerElem m widthSub p))) $
    fmap (matrix2DGetCoord m) $
    [0 .. nCols m * nRows m - 1]

solution1' :: Int -> Int -> Int -> (Int, Int)
solution1' serial width widthSub =
    let
        powerGrid = getPowerGrid serial width
        (p@(c, r), score) = maximumBy (comparing snd) $ getPowerGridMax' powerGrid widthSub
    in (c+1, r+1)

solution1 :: Int -> Int -> Int -> (Int, Int)
solution1 serial width widthSub =
    let
        powerGrid = getPowerGrid serial width
        (p, score) = getPowerGridMax powerGrid widthSub
    in p
    

solution2 :: Int -> Int -> (((Int, Int), Int), Int)
solution2 serial width =
    let
        powerGrid = getPowerGrid serial width

        maxPoints :: [(((Int, Int), Int), Int)]
        maxPoints = (\s -> (getPowerGridMax powerGrid (trace (show s) s), s)) <$> [1 .. width] 

        winner = maximumBy (comparing $ snd . fst) maxPoints

    in winner
    

main :: Int -> Int -> IO ()
main serial width = do
    putStr "Fst solution: " 
    print $ solution1 serial width 3

    putStr "Snd solution: " 
    -- TODO: Incorrect solution 
    -- print $ solution2' serial width

