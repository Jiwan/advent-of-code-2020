module Main where

import Data.List (tails, sort, find)
import Data.Maybe (isNothing, fromJust)

-- Borrowed from day 1

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

-- Solution

parseInput :: String -> [Int] 
parseInput = map read . lines

window :: Int -> [a] -> [[a]]
window n = foldr (zipWith (:)) (repeat []) . take n . tails

findSubRange :: Int -> [Int] -> [Int]
findSubRange number l =
    let firstRange = take 2 l in 
    go (drop 2 l) firstRange (sum firstRange) 
    where 
        go remainingList currentList currentSum
            | currentSum == number = currentList
            | currentSum < number = let (s:xs) = remainingList in go xs (currentList ++ [s]) (currentSum + s) 
            | currentSum > number = let (s:xs) = currentList in go remainingList xs (currentSum - s) 

main :: IO ()
main = do
    file <- readFile "data/test9.txt"
    let list = parseInput file
    putStrLn "Day 9"
    putStrLn "Part 1"
    let wrongNumber = fmap last $ find (\s -> isNothing $ findMatchingPair (take 25 s) (last s)) $ window 26 list
    print wrongNumber
    putStrLn "Part 2"
    let subRange = findSubRange (fromJust wrongNumber) list
    print $ maximum subRange + minimum subRange