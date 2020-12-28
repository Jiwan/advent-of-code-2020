module Main where

import Data.List.Split (splitOn)
import qualified Data.Set as Set

tuplify [x,y] = (x,y)

parseDecks :: String -> ([Int], [Int])
parseDecks = tuplify . map (map read . tail) . splitOn [""] . lines

play :: ([Int], [Int]) -> [Int]
play ([], r) = r
play (r, []) = r
play (s1:xs1, s2:xs2)
    | s1 > s2 = play (xs1 ++ [s1, s2], xs2)
    | s1 < s2 = play (xs1, xs2 ++ [s2, s1])

playRecursive :: ([Int], [Int]) -> Set.Set (Int, Int) -> ([Int], [Int])
playRecursive ([], r) s = ([], r)
playRecursive (r, []) s = (r, [])
playRecursive (s1:xs1, s2:xs2) roundSet 
    | hasAlreadyBeenPlayed = ([0], [])
    | s1 <= (length xs1) && s2 <= (length xs2) =
        case playRecursive (take s1 xs1, take s2 xs2) Set.empty of
            (_, []) -> playRecursive (xs1 ++ [s1, s2], xs2) newRoundSet
            ([], _) -> playRecursive (xs1, xs2 ++ [s2, s1]) newRoundSet
    | s1 > s2 = playRecursive (xs1 ++ [s1, s2], xs2) newRoundSet
    | s1 < s2 = playRecursive (xs1, xs2 ++ [s2, s1]) newRoundSet 
    where
        hash l = sum $ zipWith (*)  [1..] l
        hashOfRound = (hash (s1:xs1), hash (s2:xs2))
        hasAlreadyBeenPlayed = Set.member hashOfRound roundSet
        newRoundSet = Set.insert hashOfRound roundSet

main :: IO ()
main = do
    file <- readFile "data/test22.txt"
    putStrLn "Day 22"
    putStrLn "Part 1"
    let decks = parseDecks file
    let result = play decks
    print $ sum $ zipWith (*) (reverse result) [1..] 
    putStrLn "Part 2"
    let result2 = playRecursive decks Set.empty
    print $ sum $ zipWith (*) (reverse $ fst result2) [1..]