module Main where

import Data.List ( sort, find )

computePosition lowerSymbol upperSymbol str =
    fst $ foldl reducer (0, (2 ^ length str) - 1) str
    where 
        reducer (l, h) symbol  
            | symbol == lowerSymbol = (l, l + ((h - l) `div` 2)) 
            | symbol == upperSymbol = (l + ((h - l) `div` 2) + 1, h) 

compute2DPosition rowLength str = (computePosition 'F' 'B' (take rowLength str), computePosition 'L' 'R' (drop rowLength str))

computeSeatId rowLength str = 
    let (row, column) = compute2DPosition rowLength str in
        row * (rowLength  + 1) + column

findMaxSeatId = maximum . map (computeSeatId 7) . lines

findMissingSeat file = 
    let sortedSeats = sort . map (computeSeatId 7) $ lines file in 
        fmap ((+ 1) . fst) $ find (\(x, y) -> (y - x) > 1) $ zip sortedSeats (drop 1 sortedSeats)

main :: IO ()
main = do
    putStrLn "Day 5"
    file <- readFile "data/test5.txt"
    print $ findMaxSeatId file 
    print $ findMissingSeat file 
