module Main where

import qualified Data.Set as Set
import Data.List.Split (splitOn)

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

main :: IO ()
main = do
    putStrLn "Day 3"
    dayThreePartOne
    dayThreePartTwo

