module Main where

input = "364289715"

main :: IO ()
main = do
    file <- readFile "data/test23.txt"
    putStrLn "Day 23"
    putStrLn "Part 1"
