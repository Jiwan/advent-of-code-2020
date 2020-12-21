module Main where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
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

executeRule :: Map.Map Int [[Op]] -> [Char] -> Op  -> [String] -> Maybe [Char]
executeRule rulesMap (s:xs) (ReadOp c) backtrace
    | s == c = trace ("Successfully consuming " ++ [s]) Just xs
    | otherwise = trace ("Wrong character " ++ [s] ++ " expected " ++ [c]) Nothing
executeRule rulesMap s (ExecuteRule index) backtrace = trace ("Executing rule " ++ show index ++ "[" ++ show s ++ "]") executeRules rulesMap s (fromJust $ Map.lookup index rulesMap) backtrace

executeRules :: Map.Map Int [[Op]] -> [Char] -> [[Op]] -> [String] -> Maybe [Char]
executeRules _ [] [] backtrace = trace ("The end") Just "" 
executeRules _ [] [x] backtrace = Nothing 

--executeRules rulesMap s [[ExecuteRule 42,ExecuteRule 31], [ExecuteRule 42,ExecuteRule 11,ExecuteRule 31]] backtrace = 
--    case trace ("TADAM!!!") (executeRules rulesMap s [[ExecuteRule 42, ExecuteRule 31]] (backtrace ++ ["TEST"])) of
--        Nothing -> executeRules rulesMap s [[ExecuteRule 42,ExecuteRule 11,ExecuteRule 31]] backtrace
--        result -> trace ("Was there") result


executeRules rulesMap s [x, y] backtrace =
    case trace ("\nCHOICE: " ++ show x ++ "-" ++ show y ++ "\n") (executeRules rulesMap s [x] (backtrace ++ [show y])) of
        Nothing -> trace ("\nBacktrack " ++ show y ++ "\n") executeRules rulesMap s [y]  backtrace
        result -> trace ("Was there") result

executeRules rulesMap s [[rule]] backtrace = trace ("YOuhouh") executeRule rulesMap s rule backtrace

executeRules rulesMap s [(rule:restRule)] backtrace =
    case (executeRule rulesMap s rule backtrace) of
        Nothing -> trace "baise ouais" Nothing 
        Just rest ->  executeRules rulesMap rest [restRule] backtrace

-- executeRules rulesMap str rules
--     | null str && null rules = Just ""
--     | null str && not (null rules) = Nothing
--     | length rules == 2 = let [first, second] = rules in 
--         let result = trace ("\nCHOICE:" ++ show first  ++ "-" ++ show second ++ "\n" ++ "Testsing with " ++ show first) (executeRules rulesMap str [first]) in
--             case result of
--                 Nothing -> trace ("Failed, will backtrack with " ++ show second) executeRules rulesMap str [second]
--                 _ -> trace ("Was there") result 
--     | length rules == 1 = let (rule:restRules) = head rules in
--         let r = trace ("Executing rule" ++ show rule) (executeRule rulesMap str rule) in
--             case r of
--                 Nothing -> trace "baise ouais" Nothing 
--                 Just rest -> case restRules of
--                     [] -> Just rest
--                     _ -> trace ("Continuing with " ++ show restRules) executeRules rulesMap rest [restRules]

checkAgainstRule rulesMap s = 
    let result = executeRules rulesMap s (fromJust $ Map.lookup 0 rulesMap) [] in
        result 

count x = length . filter (==x)

rulesSecondPart = Map.fromList [
    (8,  [[ExecuteRule 42], [ExecuteRule 42, ExecuteRule 8]]),
    (11,  [[ExecuteRule 42, ExecuteRule 31], [ExecuteRule 42, ExecuteRule 11, ExecuteRule 31]])]

main :: IO ()
main = do
    -- file <- readFile "data/test19.txt"
    -- putStrLn "Day 19"
    -- putStrLn "Part 1"
    -- let (rules, strings) = parseInput file
    -- print $ count True $ map (checkAgainstRule rules) strings
    putStrLn "Part 2"
    file <- readFile "data/test19-set.txt"
    let (rules, strings) = parseInput file
    let newRules = Map.union rulesSecondPart rules
    -- print $ sortBy (\x y -> compare (fst x) (fst y)) $ Map.assocs newRules 
    print $ map (checkAgainstRule newRules) strings
    -- print $ checkAgainstRule newRules "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
    --print $ map (\s -> (checkAgainstRule newRules s, s)) strings 