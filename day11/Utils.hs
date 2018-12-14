module Utils where

import Test.Hspec
import qualified Data.Sequence as DS


data Matrix2D a = Matrix2D { nCols :: Int, nRows :: Int, vector :: DS.Seq a }

matrix2DGetIndex :: Matrix2D a -> (Int, Int) -> Int
matrix2DGetIndex m (c, r) = c * (nRows m) + r

matrix2DGetCoord :: Matrix2D a -> Int -> (Int, Int)
matrix2DGetCoord m i = divMod i (nRows m)

matrix2DSlice :: Matrix2D a -> Int -> Int -> (Int, Int) -> [a]
matrix2DSlice m@(Matrix2D nCols nRows vals) width height (c, r)
    | c + width >= nCols || r + height >= nRows = []
    | otherwise = vals'
        where
            ps = [(c', r') | c' <- [c .. c + width - 1], r' <- [r .. r + height - 1]] 
            idxs = fmap (matrix2DGetIndex m) ps
            vals' = DS.index vals <$> idxs

matrix2DGetCorners :: Matrix2D a -> Int -> (Int, Int) -> [a]
matrix2DGetCorners m width (c, r) =
    matrix2DSlice m width 1 (c, r + width - 1) ++
    matrix2DSlice m 1 (width - 1) (c + width - 1, r)

test :: IO ()
test = hspec $ do
    it "Matrix2D tests" $ do
        let m = Matrix2D 2 3 (DS.fromList [1 .. 6])
        (matrix2DGetCoord m $ matrix2DGetIndex m (0, 0)) `shouldBe` (0, 0)
        (matrix2DGetCoord m $ matrix2DGetIndex m (1, 2)) `shouldBe` (1, 2)

        (matrix2DGetCorners m 2 (0, 0)) `shouldBe` [2, 5, 4]



