module Main where

import qualified Text.Parsec as Parsec
import Data.List.Split (startsWith, splitOn)
import Data.Either (rights, fromRight)
import Data.List (isPrefixOf, find, elemIndex)
import qualified Data.Map as Map

parseField :: Parsec.Parsec String () (String, (Int, Int), (Int, Int))
parseField = do
    field <- Parsec.manyTill Parsec.anyChar (Parsec.string ": ")
    minFirstRange <- Parsec.many1 Parsec.digit
    Parsec.char '-'
    maxFirstRange <- Parsec.many1 Parsec.digit
    Parsec.string " or "
    minSecondRange <- Parsec.many1 Parsec.digit
    Parsec.char '-'
    maxSecondRange <- Parsec.many1 Parsec.digit
    return (field, (read minFirstRange, read maxFirstRange), (read minSecondRange, read maxSecondRange))

readInt :: String -> Int
readInt = read

parseData file =
    let 
        l = splitOn [""] $ lines file
        fields = rights $ map (Parsec.parse parseField "test") (l !! 0)
        ownTicket = map (readInt) $ splitOn "," ((l !! 1) !! 1)
        otherTickets = map (map readInt . splitOn ",") (tail (l !! 2))
    in
       (fields, ownTicket, otherTickets)  

buildRule (minFirstRange, maxFirstRange) (minSecondRange, maxSecondRange) x = (x >= minFirstRange && x <= maxFirstRange) || (x >= minSecondRange && x <= maxSecondRange)

buildRules :: [(a, (Int, Int), (Int, Int))] -> (Int -> Bool)
buildRules rules = \x -> (any (==True) $ map (\f -> f x) (map (\(_, firstCond, secondCond) -> buildRule firstCond secondCond) rules))

count x = length . filter (==x)

update n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs

resolveFields :: [([Bool], String)] -> [(String, Int)]
resolveFields checkedFields =
    case find ((==1) . count True . fst) checkedFields of
        Just (checks, name) -> let
                Just index = elemIndex True checks
                newFields = map (\(checks', name') -> (update index False checks', name')) checkedFields 
            in
                (name, index) : resolveFields newFields 
        Nothing -> []

main :: IO ()
main = do
    file <- readFile "data/test16.txt"
    putStrLn "Day 16"
    putStrLn "Part 1"
    let (fields, ownTicket, otherTickets) = parseData file
    let rules = buildRules fields
    let amountFields = length fields
    print $ sum $ concatMap (filter (not . rules)) otherTickets
    putStrLn "Part 2"
    let validTickets = filter (\x -> length x == amountFields) $ map (filter rules) (ownTicket:otherTickets)
    let checkColumn rule index = all (==True) $ map (\ticket -> rule $ ticket !!index) validTickets
    let checkFieldAgainstColumns firstRange secondRange = let rule = buildRule firstRange secondRange in map (\index -> (checkColumn (rule) index)) [0..(amountFields -1)]
    let checkedFields = map (\(name, firstRange, secondRange) -> (checkFieldAgainstColumns firstRange secondRange, name)) fields
    let resolvedFields = resolveFields checkedFields  
    let interestingFields = filter (isPrefixOf "departure" . fst) resolvedFields 
    print $ product $ map (\(_, index) -> ownTicket !! index) interestingFields 
