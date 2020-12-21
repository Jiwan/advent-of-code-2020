module Main where

import Data.List.Split (splitOn)
import Data.Word (Word16)
import Data.Bits (Bits(setBit, testBit))
import Data.Foldable (find)
import Data.Maybe

data Piece = Piece Word16 Word16 Word16 Word16 deriving (Show)

sideToNumber :: [Char] -> Word16
sideToNumber = foldl (\acc x -> acc * 2 + (if x == '#' then 1 else 0)) 0 

flipBits :: Word16 -> Word16
flipBits w =
    foldr (\x acc -> if testBit w x then setBit acc (9 - x) else acc) 0 [0..9]

flipV (Piece up right bottom left) =
    (Piece bottom (flipBits right) up (flipBits left)) 

flipH (Piece up right bottom left) =
    (Piece (flipBits up) left (flipBits bottom) right) 

rotateRight (Piece up right bottom left) =
    (Piece (flipBits left) up (flipBits right) bottom)

allPossibleRotationFunc = [id, flipV, flipH, flipV . flipH, rotateRight, rotateRight . flipV, rotateRight . flipH, rotateRight . flipV . flipH]

instance Show (a -> b) where
    show x = "<func>"

cornerNames = ["up", "right", "bottom", "left"]

parsePiece :: [String] -> (Int, Piece) 
parsePiece line =
    let
        pieceNumber = read $ init $ drop 5 $ head line
        shape = tail line
        up = sideToNumber $ head shape
        right = sideToNumber $ map last shape
        bottom = sideToNumber $ last shape
        left = sideToNumber $ map head shape
    in
        (pieceNumber, Piece up right bottom left)

match (Piece up1 right1 bottom1 left1) (Piece up2 right2 bottom2 left2) = 
    let
        cornerPieces1 = zip [up1, right1, bottom1, left1] cornerNames
        cornerPieces2 = zip [up2, right2, bottom2, left2] cornerNames
    in
        map (\(side1, sideName1) -> (sideName1, map snd $ filter ((==side1) . fst) cornerPieces2)) cornerPieces1

-- matchInAllDirections piece1 piece2 = 
--         map (\f -> let rotatedPiece2 = f piece2 in (name, matchOneCorner side1 rotatedPiece2, f)) allPossibleRotationFunc


parsePuzzle = map parsePiece . splitOn [""] . lines 




main :: IO ()
main = do
    file <- readFile "data/test20.txt"
    putStrLn "Day 20"
    putStrLn "Part 1"
    let puzzle = parsePuzzle file
    print $ puzzle
    print $ match (snd (puzzle !! 0)) (snd (puzzle !! 0))