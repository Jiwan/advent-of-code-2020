module Main where

import Data.List ( sort )

-- Day 1 --

findMatchingPair :: (Num a, Ord a) => [a] -> a -> Maybe (a, a)
findMatchingPair list expected = 
    let sortedList = sort list
    in go sortedList (reverse sortedList)
    where
        go list1@(head1:tail1) list2@(head2:tail2) 
            | sum < expected  = go tail1 list2 
            | sum > expected = go list1 tail2
            | sum < 0 = Nothing
            | otherwise = Just (head1, head2)
            where sum = head1 + head2
        go _ _ = Nothing

findMatchingTriplet (head:tail) expected = 
    case findMatchingPair tail (expected - head) of
        Just(x, y) -> Just(head, x, y)
        _ -> findMatchingTriplet tail expected

findMatchingTriplet _ _ = Nothing

readNumbers filename = do
    file <- readFile filename
    return $ map read $ words file

dayOnePartOne = do 
    numbers <- readNumbers "data/test1.txt"
    let pair = findMatchingPair numbers 2020
    putStrLn $ case pair of 
        Just (x, y) -> show $ x * y 
        _ -> "Couldn't find any pair"

dayOnePartTwo = do
    numbers <- readNumbers "data/test1.txt"
    let pair = findMatchingTriplet numbers 2020
    putStrLn $ case pair of 
        Just (x, y, z) -> show $ x * y * z
        _ -> "Couldn't find any pair"

main :: IO ()
main = do
    putStrLn "Day 1"
    dayOnePartOne
    dayOnePartTwo