module Lib
    (findMatchingPair,
    findMatchingTriplet,
    parsePoliciesAndPasswords,
    verifyPassword,
    verifyPasswordV2,
    verifyAllPasswords,
    parseForest
    ) where

import Data.List ( sort )
import Data.List.Split (splitOneOf)
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

-- Day 3 --
parseTrees = map (map fst . filter (\(_, symb) -> symb == '#') . zip [0..]) . lines

parseForest string = (length . head $ lines string, parseTrees string)

checkTreeColision verticalDirection rowWidth treeRow (pos, amountTreeCollision) =  (0, 0, 0)-- pos `mod` rowWidth

slide (horizontalDirection, verticalDirection) (forestSize, trees) = 
    foldl (checkTreeColision verticalDirection forestSize) (0, 0, 0) trees