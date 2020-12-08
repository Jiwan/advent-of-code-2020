module Main where

import Data.List ( nub )
import Data.List.Split (splitOn)
import qualified Data.Map as Map

partOne = sum . map (length . nub . concat) . splitOn [""] . lines

frequency str = (Map.fromListWith (+) [(c, 1) | c <- str])

partTwo file =
    let groupsFreq = map (\group -> (frequency $ concat group, length group)) . splitOn [""] $ lines file in
        sum $ map (length . (\(f, l) -> Map.filter (==l) f)) groupsFreq

main :: IO ()
main = do
    putStrLn "Day 6"
    file <- readFile "data/test6.txt"
    print $ partOne file
    print $ partTwo file
