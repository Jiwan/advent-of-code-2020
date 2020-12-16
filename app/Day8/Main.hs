module Main where

import Data.List.Split (splitOn)
import qualified Control.Monad.State as State
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Ord (comparing)

-- Helpers to have a take depending a state. 
isDuplicate :: Ord a => a -> State.State (Set.Set a) Bool
isDuplicate x = do
    alreadySeen <- State.get
    if Set.member x alreadySeen
    then return True 
    else do
        State.modify (Set.insert x)
        return False

takeUntilM :: Monad m => [a] -> (a -> m Bool) -> m [a]
takeUntilM (s:xs) p = do 
    isEnd <- p s
    if not isEnd
    then do
        rest <- takeUntilM xs p
        return (s:rest) 
    else return []
takeUntilM [] _ = return []

-- Solution
parseInstructions = map (\line -> let x = splitOn " " line in (x !! 0, read $ dropWhile (=='+') (x !! 1) :: Int)) . lines
 
stepOnce (ip, reg0) instructions =
    case instructions !! ip of 
        ("jmp", offset) -> (ip + offset, reg0)
        ("acc", value) -> (ip + 1, reg0 + value)
        ("nop", _) -> (ip + 1, reg0)

trace (ip, reg0) instructions =
    if ip < length instructions
        then (ip, reg0) : trace (stepOnce (ip, reg0) instructions) instructions
        else [(ip, reg0)]

traceUntilFirstDuplicated instructions (ip, reg0) = 
    State.evalState (takeUntilM (trace (ip, reg0) instructions) (isDuplicate . fst)) Set.empty 

flipInstruction index instructions = take index instructions ++ [flip (instructions !! index)] ++ drop (index + 1) instructions
    where
        flip ("jmp", offset) = ("nop", offset)
        flip ("nop", offset) = ("jmp", offset)

traceFrom instructions (ip, reg0) = 
    let newInstructions = flipInstruction ip instructions in 
     traceUntilFirstDuplicated newInstructions (ip, reg0)

main :: IO ()
main = do
    file <- readFile "data/test8.txt"
    let instructions = parseInstructions file
    putStrLn "Day 8"
    putStrLn "Part 1"
    let firstTrace = traceUntilFirstDuplicated instructions (0,0)
    print firstTrace 
    putStrLn "Part 2"
    let buggyOps = filter (\(ip, _) -> fst (instructions !! ip) `elem` ["jmp", "nop"]) firstTrace
    print $ last $ List.maximumBy (comparing last) $ List.map (traceFrom instructions) buggyOps
