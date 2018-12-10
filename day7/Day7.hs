module Day7 where

import System.IO
import qualified Text.Trifecta as TT
import Data.List
import Data.List.Unique (sortUniq)
import Data.Char (ord)

data Step = Step { name :: Char, dependant :: Char } deriving (Show, Eq, Ord)

stepParser :: TT.Parser Step
stepParser = do
    name <- TT.string "Step " *> TT.anyChar <* TT.string " must be finished before step "
    dependant <- TT.anyChar <* TT.string " can begin." <* TT.newline
    return $ Step name dependant


parseInput :: String -> TT.Result [Step]
parseInput = TT.parseString (TT.some $ stepParser) mempty

getIndependants :: [Step] -> [Char]
getIndependants steps =
    let
        names = fmap name steps
        dependants = fmap dependant steps
        independants = filter (\x -> not $ elem x dependants) names
    in
        sortUniq $ independants

removeTasks :: [Step] -> [Char] -> [Step]
removeTasks steps n = filter (\s -> not $ elem (name s) n) steps

sortDepending :: [Step] -> [Char]
sortDepending steps = reverse $ go [] steps 
    where
        go l [] = l
        go l steps =
            let
                nextTask = head $ getIndependants steps
            in
                go (nextTask : l) (removeTasks steps [nextTask])

getFinalSteps :: [Step] -> [Step]
getFinalSteps steps =
    let dependants = sortUniq $ fmap dependant steps
        taskDone = '#'
    in fmap (\x -> (Step x taskDone)) dependants

sortTasks :: [Step] -> [Char]
sortTasks steps = sortDepending $ steps

getDuration :: Char -> Int
getDuration c = ord c - ord 'A' + 1

getDurationTasks :: [Step] -> Int -> Int -> Int
getDurationTasks steps nWorkers extraTime = go [] 0 steps
    where
        oneSecWork :: [(Char, Int)] -> [(Char, Int)]
        oneSecWork = fmap (\(c, d) -> (c, d - 1)) .  take nWorkers
        
        go ongoing t steps
            | null ongoing && null steps = t
            | otherwise =
                let
                    nNewTasks = nWorkers - length ongoing
                    newTasks =
                        take nNewTasks $
                        filter (\x -> not $ elem x (fmap fst ongoing)) $
                        getIndependants steps
                    ongoing' = ongoing ++ fmap (\c -> (c, extraTime + getDuration c)) newTasks 
                    (done, ongoing'') = partition (\(_, d) -> d == 0) $ oneSecWork ongoing' 
                    newSteps = removeTasks steps (fmap fst done)
                in
                    go ongoing'' (t + 1) newSteps


main :: String -> Int -> Int -> IO ()
main filename nWorkers extraTime = do
inh <- openFile filename ReadMode
inpStr <- hGetContents inh

case (parseInput inpStr) of
    TT.Failure e -> print e

    TT.Success steps -> do
        let steps' = steps ++ getFinalSteps steps
        putStr "First solution: "
        print $ sortTasks steps'

        putStr "Snd solution: "
        print $ getDurationTasks steps' nWorkers extraTime

hClose inh

