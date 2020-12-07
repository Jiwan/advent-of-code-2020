module Main where

import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List (isSuffixOf)
import Text.Read (readMaybe)
import Data.Maybe
import Data.Char (isDigit)

-- Day 4 --

tuplify2 [x,y] = (x,y)

data Field = Field { name :: String, check :: String -> Bool }

parseFields = map (tuplify2 . splitOn ":") . words

parsePassports = map (Map.fromList . concatMap parseFields) . splitOn [""] . lines

checkNumber min max str =
    let number = readMaybe str :: Maybe Int in
        fromMaybe False $ fmap (\num -> num >= min && num <= max) number

checkEyeColor str = elem str ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] 

checkPassportId str = length str == 9 && all isDigit str 

checkHairColor (x:xs) 
    | x == '#' && all (\x -> elem x ['0'..'9'] || elem x ['a'..'f']) xs = True
    | otherwise = False

discardNLast n str = (take ((length str) - n) str)

checkHeight str 
    | "cm" `isSuffixOf` str = checkNumber 150 193 (discardNLast 2 str)
    | "in" `isSuffixOf` str = checkNumber 59 76 (discardNLast 2 str)
    | otherwise = False

requiredFields = 
    [ (Field "byr" $ checkNumber 1920 2002)
    , (Field "iyr" $ checkNumber 2010 2020)
    , (Field "eyr" $ checkNumber 2020 2030)
    , (Field "hgt" $ checkHeight)
    , (Field "hcl" $ checkHairColor)
    , (Field "ecl" $ checkEyeColor)
    , (Field "pid" $ checkPassportId)]

checkField passport field = fromMaybe False $ fmap (check field) (Map.lookup (name field) passport) 

checkIsValidPassport passport = all (checkField passport) requiredFields

countValidPassports = length . filter (==True) . map checkIsValidPassport

main :: IO ()
main = do
    putStrLn "Day 4"
    file <- readFile "data/test4.txt"
    print $ countValidPassports $ parsePassports file

