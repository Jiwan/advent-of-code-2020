module Main where

import Data.List.Split (splitOn)
import Data.Word (Word16)
import Data.Bits (Bits(setBit, testBit))
import Data.Foldable (find)
import Data.Maybe
import Data.List (groupBy)

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
            testedCorners = zip cornerNames $ map (uncurry (==)) $ zip [up1, right1, bottom1, left1] [bottom2, left2, up2, right2] 
        in
            map fst $ filter ((==True) . snd) testedCorners 


matchInAllDirections piece1 piece2 = 
    let matches = map (\f -> let rotatedPiece2 = f piece2 in (match piece1 rotatedPiece2, f)) allPossibleRotationFunc in
        filter (\(m, _) -> not $ null m) matches

matchAllPieces pieces = 
    let 
        allMatches = [(pieceId1, (pieceId2, matchInAllDirections shape1 shape2)) | (pieceId1, shape1) <- pieces, (pieceId2, shape2) <- pieces, pieceId1 /= pieceId2]
        nonEmptyMatches = filter (\(_, ( _, matches)) -> not $ null matches) allMatches
        groupedMatches = groupBy (\x y -> fst x == fst y) nonEmptyMatches
    in
        map (\(s:xs) -> (fst s, map snd (s:xs))) groupedMatches

parsePuzzle = map parsePiece . splitOn [""] . lines 


main :: IO ()
main = do
    file <- readFile "data/test20.txt"
    putStrLn "Day 20"
    putStrLn "Part 1"
    let puzzle = parsePuzzle file
    let allMatches = matchAllPieces puzzle 
    let corners = filter ((==2) . length . snd) allMatches
    print corners
    print $ product $ map fst corners 