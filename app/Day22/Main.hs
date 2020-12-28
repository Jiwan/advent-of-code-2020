module Main where

import Data.List.Split (splitOn)

tuplify [x,y] = (x,y)

parseDecks :: String -> ([Int], [Int])
parseDecks = tuplify . map (map read . tail) . splitOn [""] . lines

play :: ([Int], [Int]) -> [Int]
play ([], r) = r
play (r, []) = r
play (s1:xs1, s2:xs2)
    | s1 > s2 = play (xs1 ++ [s1, s2], xs2)
    | s1 < s2 = play (xs1, xs2 ++ [s2, s1])

main :: IO ()
main = do
    file <- readFile "data/test22.txt"
    putStrLn "Day 22"
    putStrLn "Part 1"
    let decks = parseDecks file
    let result = play decks
    print $ sum $ zipWith (*) (reverse result) [1..] 