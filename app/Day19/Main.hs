module Main where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad (msum)

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

executeRule :: Map.Map Int [[Op]] -> [Char] -> Op -> Maybe [Char]
executeRule rulesMap (s:xs) (ReadOp c)
    | s == c = Just xs
    | otherwise = Nothing
executeRule rulesMap s (ExecuteRule index) = executeRules rulesMap s (fromJust $ Map.lookup index rulesMap)

executeRules :: Map.Map Int [[Op]] -> [Char] -> [[Op]] -> Maybe [Char]
executeRules _ [] _ = Just ""
executeRules rulesMap s [x, y] =
    msum [(executeRules rulesMap s [x]), (executeRules rulesMap s [y])]
executeRules rulesMap s [[rule]] = executeRule rulesMap s rule
executeRules rulesMap s [(rule:restRule)] = do
    r <- (executeRule rulesMap s rule)
    executeRules rulesMap r [restRule]
executeRules rulesMap s rules = Just (s ++ " " ++ show rules)

checkAgainstRule rulesMap s = 
    let result = executeRules rulesMap s (fromJust $ Map.lookup 0 rulesMap) in
        (result == (Just ""), s)

count x = length . filter (==x)

rulesSecondPart = Map.fromList [(8,  [[ExecuteRule 42], [ExecuteRule 42, ExecuteRule 8]]), (11,  [[ExecuteRule 42, ExecuteRule 31], [ExecuteRule 42, ExecuteRule 11, ExecuteRule 31]])]

main :: IO ()
main = do
    file <- readFile "data/test19-set.txt"
    putStrLn "Day 19"
    putStrLn "Part 1"
    let (rules, strings) = parseInput file
    --print $ count True $ map (checkAgainstRule rules) strings
    let newRules = Map.union rulesSecondPart rules
    print $ filter ((==True) . fst) $ map (checkAgainstRule newRules) strings