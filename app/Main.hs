module Main where

import Lib (
    findMatchingPair,
    findMatchingTriplet,
    parsePoliciesAndPasswords,
    verifyPassword,
    verifyPasswordV2,
    verifyAllPasswords,
    parseForest
    )

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

dayTwoPartOne = do
    file <- readFile "data/test2.txt"
    print $ verifyAllPasswords verifyPassword $ parsePoliciesAndPasswords file 

dayTwoPartTwo = do
    file <- readFile "data/test2.txt"
    print $ verifyAllPasswords verifyPasswordV2 $ parsePoliciesAndPasswords file 

dayThreePartOne = do
    file <- readFile "data/test3.txt"
    print $ parseForest file 

main :: IO ()
main = do
    putStrLn "Day 1"
    dayOnePartOne
    dayOnePartTwo
    putStrLn "Day 2"
    dayTwoPartOne
    dayTwoPartTwo
    putStrLn "Day 3"
    dayThreePartOne

