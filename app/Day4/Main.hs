module Main where

import qualified Data.Map as Map
import Data.List.Split (splitOn)

-- Day 4 --

tuplify2 [x,y] = (x,y)

parseFields = map (tuplify2 . splitOn ":") . words

parsePassports = map (Map.fromList . concatMap parseFields) . splitOn [""] . lines

checkIsValidPassport passport = all (`Map.member` passport) requiredFields
    where requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

countValidPassports = length . filter (==True) . map checkIsValidPassport

dayFourPartOne = do
    file <- readFile "data/test4.txt"
    print $ countValidPassports $ parsePassports file


main :: IO ()
main = do
    putStrLn "Day 4"
    dayFourPartOne

