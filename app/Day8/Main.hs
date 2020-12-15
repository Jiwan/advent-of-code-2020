module Main where

import Data.List.Split (splitOn)

parseInstructions = map (\line -> let x = splitOn " " line in (x !! 0, read $ dropWhile (=='+') (x !! 1) :: Int)) . lines
 
stepOnce (ip, reg0) instructions =
    case instructions !! ip of 
        ("jmp", offset) -> (ip + offset, reg0)
        ("acc", value) -> (ip + 1, reg0 + value)
        ("nop", _) -> (ip + 1, reg0)

steps (ip, reg0) instructions =
    (ip, reg0) : steps (stepOnce (ip, reg0) instructions) instructions

main :: IO ()
main = do
    file <- readFile "data/test8.txt"
    putStrLn "Day 8"
    putStrLn "Part 1"
    let instructions = parseInstructions file
    print $ take 4 $ steps (0, 0) instructions



