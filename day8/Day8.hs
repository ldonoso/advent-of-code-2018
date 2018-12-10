module Day8 where

import System.IO
import Text.Trifecta

parseInput :: String -> Result [Int]
parseInput = parseString (some $ fromInteger <$> integer) mempty 

data Tree a = Tree { children :: [Tree a], meta :: a} deriving (Show)

getValue :: Tree [Int] -> Int
getValue (Tree [] meta) = sum meta
getValue (Tree children meta) =
    let
        children' = fmap (children !!) . filter (length children >) . fmap (subtract 1) $ meta
    in 
        sum . fmap getValue $ children'

instance Foldable Tree where
    foldr fabb b (Tree [] meta) = fabb meta b
    foldr fabb b (Tree (x:xs) meta) =
        foldr fabb b' (Tree xs meta)
        where
            b' = foldr fabb b x


fromList :: [Int] -> ([Int], [Tree [Int]])
fromList xs = go 1 xs []
    where 
        go 0 xs trees = (xs, reverse trees)
        go n (nChil : nMet : xs ) trees =
            let 
                (rest, children) = go nChil xs []
            in
                go (n - 1) (drop nMet rest) (Tree children (take nMet rest) : trees)

main :: String -> IO ()
main filename = do
    inh <- openFile filename ReadMode
    inpStr <- hGetContents inh

    case (parseInput inpStr) of
        Failure e -> print e

        Success xs -> do
            let tree =  head . snd $ fromList xs

            putStr "Fst solution: "
            print $ foldr (\x acc -> foldr (+) acc x) 0 tree

            putStr "Snd solution: "
            print $ getValue tree

    hClose inh

