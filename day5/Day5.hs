module Day5 where

import System.IO
import Data.Monoid
import Data.List
import Data.List.Unique (sortUniq)
import Data.Ord (comparing)
import Control.Applicative
import Text.Pretty.Simple (pPrint)
import Data.Char (toUpper, toLower)


reducePolymer :: String -> String
reducePolymer = reverse . go []
    where
        go left [] = left
        go left (x : []) = x : left
        go left (x : y : xs) =
            case (canBeReduced x y) of
                True ->
                    case left of
                        (l : ls) -> go ls (l : xs)
                        [] -> go [] xs
                False -> go (x : left) (y : xs) 

        canBeReduced :: Char -> Char -> Bool
        canBeReduced x y = (x /= y) && (toUpper x == toUpper y)

main :: String -> IO ()
main filename = do
    inh <- openFile filename ReadMode
    polymereAll <- hGetLine inh

    let polymere = reducePolymer polymereAll
    print "First solution: "
    print . length $ polymere

    -- The affected letter must be among the ones in the reduced polymere
    let uniqueLetters = sortUniq polymere

    let f x = length . reducePolymer . filter (/= toLower(x)) . filter (/= toUpper(x)) $ polymereAll
    print "Second solution: "
    print . minimum . fmap f $ uniqueLetters

    hClose inh

