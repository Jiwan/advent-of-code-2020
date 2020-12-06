module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List ( sort )
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (catMaybes)

-- Day 1 --

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

findMatchingTriplet (head:tail) expected = 
    case findMatchingPair tail (expected - head) of
        Just(x, y) -> Just(head, x, y)
        _ -> findMatchingTriplet tail expected

findMatchingTriplet _ _ = Nothing

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

-- Day 2 --

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

dayTwoPartOne = do
    file <- readFile "data/test2.txt"
    print $ verifyAllPasswords verifyPassword $ parsePoliciesAndPasswords file 

dayTwoPartTwo = do
    file <- readFile "data/test2.txt"
    print $ verifyAllPasswords verifyPasswordV2 $ parsePoliciesAndPasswords file 

-- Day 3 --

data Forest = Forest { size :: Int, trees :: [Set.Set Int] } deriving (Show)
data Direction = Direction {x :: Int, y :: Int} deriving (Show)  

parseTrees = map (Set.fromList . map fst . filter (\(_, symb) -> symb == '#') . zip [0..]) . lines

parseForest string = Forest (length . head $ lines string) (parseTrees string)

takeNth n (head:tail) =
    head : go n tail
    where 
        go n list = case drop (n - 1) list of
            (h:t) -> h : go n t
            [] -> []

checkTreeColision horizontalDirection rowWidth (pos, amountTreeCollision) row = 
    let collided = Set.member (pos `mod` rowWidth) row in 
        (pos + horizontalDirection, amountTreeCollision + fromEnum collided)

slide (Forest size trees) (Direction x y) = 
    let rows = takeNth y trees in
        foldl (checkTreeColision x size) (0, 0) rows 

dayThreePartOne = do
    file <- readFile "data/test3.txt"
    print $ snd $ slide (parseForest file) (Direction 3 1) 

dayThreePartTwo = do
    file <- readFile "data/test3.txt"
    let forest = parseForest file
    print $ product $ map (snd. slide forest) directions
    where directions = [Direction 1 1, Direction 3 1, Direction 5 1, Direction 7 1, Direction 1 2]

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
    putStrLn "Day 1"
    dayOnePartOne
    dayOnePartTwo
    putStrLn "Day 2"
    dayTwoPartOne
    dayTwoPartTwo
    putStrLn "Day 3"
    dayThreePartOne
    dayThreePartTwo
    putStrLn "Day 4"
    dayFourPartOne

