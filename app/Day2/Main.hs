module Main where

import Data.List.Split (splitOneOf)
import Data.Maybe (catMaybes)

count value = length . filter (==value)

parsePolicyAndPassword :: [Char] -> Maybe (Int, Int, Char, [Char])
parsePolicyAndPassword string = 
    case splitOneOf [':', ' ', '-'] string of 
        (min:max:character:_:password) -> Just (read min, read max, head character, head password)
        _ -> Nothing

parsePoliciesAndPasswords = map parsePolicyAndPassword . lines

verifyPassword (min, max, character, password) =
    let c = count character password in
        c >= min && c <= max

verifyPasswordV2 (offset1, offset2, character, password) =
    (password !! (offset1 - 1) == character) /= (password !! (offset2 - 1) == character)

verifyAllPasswords :: Eq c => ((Int, Int, c, [c]) -> Bool) -> [Maybe (Int, Int, c, [c])] -> Int
verifyAllPasswords predicate = count True . map predicate . catMaybes 

main :: IO ()
main = do
    putStrLn "Day 2"
    putStrLn "Part 1"
    file <- readFile "data/test2.txt"
    print $ verifyAllPasswords verifyPassword $ parsePoliciesAndPasswords file 
    putStrLn "Part 2"
    print $ verifyAllPasswords verifyPasswordV2 $ parsePoliciesAndPasswords file 