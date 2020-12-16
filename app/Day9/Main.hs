module Main where

main :: IO ()
main = do
    file <- readFile "data/test8.txt"
    putStrLn "Day 8"
    putStrLn "Part 1"
s