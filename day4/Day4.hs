module Day4 where

import System.IO
import Text.Trifecta
import Data.Monoid
import Data.List
import Data.Ord (comparing)
import Control.Applicative
import Text.Pretty.Simple (pPrint)

data Date = Date { day :: String, hour :: Int, min :: Int} deriving (Show, Eq, Ord)
data EventType = Wakes | Sleeps | Begins Int deriving (Show)
data Event = Event { date :: Date, eventType :: EventType } deriving (Show)
data SleepPeriod = SleepPeriod { id :: Int, minStart :: Int, minEnd :: Int } deriving (Show)

dateParser :: Parser Date
dateParser = do
    char '['
    day <- (some $ notChar ' ') <* whiteSpace
    hour <- fromInteger <$> integer
    char ':'
    min <- fromInteger <$> integer
    string "] "
    return $ Date day hour min

eventParser :: Parser Event
eventParser = do
    d <- dateParser
    typeEvent <- token eventTypeParser
    return $ Event d typeEvent

eventTypeParser :: Parser EventType
eventTypeParser = (parserWakes <|> parserSleeps <|> parserBegins)
    where
        parserWakes = string "wakes up" *> pure Wakes
        parserSleeps = string "falls asleep" *> pure Sleeps
        parserBegins = do
            id <- string "Guard #" *> integer <* string "begins shift"
            return $ Begins (fromInteger $ id)

parseInput :: String -> Result [Event]
parseInput = parseString (some $ token $ eventParser) mempty


toSleepPeriods :: [Event] -> [SleepPeriod]
toSleepPeriods [] = []
toSleepPeriods ((Event _ (Begins id)) : xs) = go id xs
    where
        go id ((Event d1 Sleeps) : (Event d2 Wakes) : xs) =
            (SleepPeriod id (Day4.min d1) (Day4.min d2)) : go id xs
        go _ xs = toSleepPeriods xs
toSleepPeriods xs = error $ "Invalid " ++ show xs

aggregatePeriods :: [SleepPeriod] -> Int
aggregatePeriods = foldr (+) 0 . fmap (\s -> (minEnd s) - (minStart s))

mostFrequentMin :: [SleepPeriod] -> (Int, Int)
mostFrequentMin =
    maximumBy (comparing snd) . frequencies
    where
        expand :: SleepPeriod -> [Int]
        expand (SleepPeriod _ minStart minEnd) = [minStart .. (minEnd -1)]

        frequencies = getFrequencies . concat . fmap expand

getFrequencies :: (Eq a, Ord a) => [a] -> [(a, Int)]
getFrequencies = fmap (\x -> (head x, length x)) . group . sort

main :: String -> IO ()
main filename = do
    inh <- openFile filename ReadMode
    inpStr <- hGetContents inh

    case (parseInput inpStr) of
        Failure e -> print e

        Success events -> do
            putStr "First solution: "
            let sortedEvents = sortBy (\l r -> compare (date l) (date r)) $ events
            let sleepPeriods = groupBy (\l r -> (Day4.id l) == (Day4.id r)) . sortBy (\l r -> compare (Day4.id l) (Day4.id r)) . toSleepPeriods $ sortedEvents

            let sleepPeriodMax = maximumBy (comparing aggregatePeriods) $ sleepPeriods
            print $ (Day4.id . head $ sleepPeriodMax) * (fst $ mostFrequentMin sleepPeriodMax)
            
            let sleepPeriodsMostOften = fmap mostFrequentMin sleepPeriods 
            let solution2 = zip (fmap (Day4.id . head) sleepPeriods) sleepPeriodsMostOften
            let solution2' = maximumBy (comparing (snd . snd)) solution2
            putStr "Second solution: "
            print $ (fst solution2') * (fst . snd $ solution2')



    hClose inh

