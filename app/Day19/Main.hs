module Main where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, fromJust)
import Control.Monad (msum)
import Debug.Trace (trace)
import Data.List (sortBy)

data Op = ReadOp Char | ExecuteRule Int deriving (Show)

parseOp ('"':s:xs) = ReadOp s
parseOp xs = ExecuteRule (read xs :: Int)

parseRule line =
    let
        rule = (takeWhile (not . (==':')) line)
        rest = drop (length rule + 1) line
        choices = map (map parseOp . filter (not . (=="")) . splitOn " ") $ splitOn "|" rest
    in
        (read rule :: Int, choices)

parseRules = Map.fromList . map parseRule

parseInput file = 
    let
        l = splitOn [""] $ lines file
    in
        (parseRules $ head l, head (tail l))

executeRule :: Map.Map Int [[Op]] -> Op -> [Char] -> [Maybe [Char]]
executeRule rulesMap (ReadOp c) (s:xs) 
    | s == c = [Just xs]
    | otherwise = [Nothing]
executeRule rulesMap (ExecuteRule index) s = executeOrRules rulesMap (fromJust $ Map.lookup index rulesMap) [s]

executeOrRules :: Map.Map Int [[Op]] -> [[Op]] -> [[Char]] -> [Maybe [Char]]
executeOrRules rulesMap orRules stringsToCheck
    | "" `elem` stringsToCheck = []
    | otherwise = concat $ concatMap (\s -> map (\op -> executeSeqRules rulesMap op s) orRules) stringsToCheck

executeSeqRules :: Map.Map Int [[Op]] -> [Op] -> [Char] -> [Maybe [Char]]
executeSeqRules rulesMap [rule] s = executeRule rulesMap rule s
executeSeqRules rulesMap (rule:restRule) s =
    let 
        results = executeRule rulesMap rule s
        filteredResults = catMaybes results  
    in
        concat $ map (executeSeqRules rulesMap restRule) filteredResults 

checkAgainstRule rulesMap s = 
    let result = executeOrRules rulesMap (fromJust $ Map.lookup 0 rulesMap) [s] in
        (Just "" `elem` result) 

count x = length . filter (==x)

rulesSecondPart = Map.fromList [
    (8,  [[ExecuteRule 42], [ExecuteRule 42, ExecuteRule 8]]),
    (11,  [[ExecuteRule 42, ExecuteRule 31], [ExecuteRule 42, ExecuteRule 11, ExecuteRule 31]])]

main :: IO ()
main = do
    file <- readFile "data/test19.txt"
    putStrLn "Day 19"
    putStrLn "Part 1"
    let (rules, strings) = parseInput file
    print $ count True $ map (checkAgainstRule rules) strings
    putStrLn "Part 2"
    let newRules = Map.union rulesSecondPart rules
    print $ count True $ map (checkAgainstRule newRules) strings