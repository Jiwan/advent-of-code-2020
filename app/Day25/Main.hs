module Main where


main :: IO ()
main = do
    file <- readFile "data/test25.txt"
    putStrLn "Day 25"
    putStrLn "Part 1"
