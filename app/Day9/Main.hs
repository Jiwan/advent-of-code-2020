module Main where

main :: IO ()
main = do
    file <- readFile "data/test8.txt"
    putStrLn "Day 9"
    putStrLn "Part 1"
s