module Main where

import qualified Data.Map as Map

puzzleInput = [20,0,1,11,6,3]

play :: [Int] -> Int -> Int 
play list amountOfTurns =
    let initList = take (length list - 1) list in 
    go (Map.fromList (zip initList [1..])) (last list) (length list + 1)
    where
        go map numberSpoken turn
            | turn == amountOfTurns = numberSpoken
            | otherwise = case Map.lookup numberSpoken map of 
                Just lastTime -> let number = ((turn - 1) - lastTime) in go (Map.insert numberSpoken (turn - 1) map) number (turn + 1)
                Nothing -> go (Map.insert numberSpoken (turn - 1) map) 0 (turn + 1)

main :: IO ()
main = do
    putStrLn "Day 15"
    putStrLn "Part 1"
    print $ play puzzleInput 2021 
    print $ play puzzleInput 30000001 
