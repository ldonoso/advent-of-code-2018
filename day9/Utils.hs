module Utils where

import qualified Data.Sequence as DS
import Test.Hspec

rotate :: Int -> DS.Seq a -> DS.Seq a
rotate n xs
    | DS.null xs = xs
    | otherwise = 
        if n < 0 then
            rotate ((DS.length xs) + n) xs
        else
            (DS.drop n xs) DS.>< (DS.take n xs)

reduceList :: Int -> [a] -> [a]
reduceList step xs = [x | (i, x) <- zip [0..] xs, i `mod` step == 0]

test :: IO ()
test = hspec $ do
    it "Rotate tests" $ do
        rotate 1 (DS.fromList "abcd") `shouldBe` (DS.fromList "bcda")
        rotate (-1) (DS.fromList "abcd") `shouldBe` (DS.fromList "dabc")
        rotate (-7) (DS.fromList "abcd") `shouldBe` (DS.fromList "bcda")
