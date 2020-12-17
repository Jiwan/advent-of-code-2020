module Main where

import Data.List.Split (splitOn)
import Data.Foldable (minimumBy)
import Data.Ord (comparing)

parseData file = 
    let 
        l = lines file
        departTime =  read $ head l :: Int
        buses = map (\x -> read x ::Int) . filter (/="x") $ splitOn "," $ l !! 1
    in
        (departTime, buses)

main :: IO ()
main = do
    file <- readFile "data/test13.txt"
    putStrLn "Day 13"
    putStrLn "Part 1"
    let (departTime, buses) = parseData file
    let (bus, time) = minimumBy (comparing snd) $ map (\x -> let m = departTime `mod` x in (x, (x - m))) buses
    print $ bus * time
    putStrLn "Part 2"