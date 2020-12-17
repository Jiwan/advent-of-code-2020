module Main where

import Data.List (sort, group, product, length, product)

parseInput :: String -> [Int] 
parseInput = map read . lines

count x =  length . filter (==x)

combination 0 = 1
combination 1 = 1
combination 2 = 2
combination n = (combination (n - 1)) + (combination (n - 2)) + (combination (n - 3))


main :: IO ()
main = do
    file <- readFile "data/test10.txt"
    putStrLn "Day 10"
    putStrLn "Part 1"
    let sortedAdapters = 0 : sort ( parseInput file)
    let diffs = zipWith (-) (tail sortedAdapters) sortedAdapters 
    print $ (count 3 diffs + 1) * count 1 diffs
    putStrLn "Part 2"
    print $ product $ map (combination . length) $ filter ((==1) . head) $ group diffs

