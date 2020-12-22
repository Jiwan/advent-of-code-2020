module Main where

import Data.List.Split (splitOn)
import Data.Word (Word16)
import Data.Bits (Bits(setBit, testBit))
import Data.Foldable (find)
import Data.Maybe
import Data.List (transpose, groupBy)
import qualified Data.Map as Map

data BorderName = UpBorder | RightBorder | BottomBorder | LeftBorder deriving (Show)

allBorderNames = [UpBorder, RightBorder, BottomBorder, LeftBorder]

data PieceBorders = PieceBorders Word16 Word16 Word16 Word16 deriving (Show)

data Piece = Piece PieceBorders [String] deriving (Show)

sideToNumber :: [Char] -> Word16
sideToNumber = foldl (\acc x -> acc * 2 + (if x == '#' then 1 else 0)) 0 

flipBits :: Word16 -> Word16
flipBits w =
    foldr (\x acc -> if testBit w x then setBit acc (9 - x) else acc) 0 [0..9]

flipBorderV (PieceBorders up right bottom left) =
    (PieceBorders bottom (flipBits right) up (flipBits left)) 

flipBorderH (PieceBorders up right bottom left) =
    (PieceBorders (flipBits up) left (flipBits bottom) right) 

rotateBorderRight (PieceBorders up right bottom left) =
    (PieceBorders (flipBits left) up (flipBits right) bottom)

flipContentV = reverse

flipContentH = map reverse

rotateContentRight = map reverse . reverse . transpose

data RotateFuncs = RotateFuncs { rotateBorders :: PieceBorders -> PieceBorders, rotateContent :: [String] -> [String]} deriving (Show)
allPossibleRotationFunc :: [RotateFuncs]
allPossibleRotationFunc = [
    RotateFuncs id id, 
    RotateFuncs flipBorderV flipContentV,
    RotateFuncs flipBorderH flipContentH,
    RotateFuncs (flipBorderV . flipBorderH) (flipContentV . flipContentH),
    RotateFuncs rotateBorderRight rotateContentRight,
    RotateFuncs (rotateBorderRight . flipBorderV) (rotateContentRight . flipContentV),
    RotateFuncs (rotateBorderRight . flipBorderH) (rotateContentRight . flipContentH),
    RotateFuncs (rotateBorderRight . flipBorderV . flipBorderH) (rotateContentRight . flipContentV . flipContentH)]

instance Show (a -> b) where
    show x = "<func>"

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
        (pieceNumber, Piece (PieceBorders up right bottom left) (map (init . tail) $ init $ tail shape))

parsePuzzle :: String -> [(Int, Piece)]
parsePuzzle = map parsePiece . splitOn [""] . lines 

match :: PieceBorders -> PieceBorders -> [BorderName]
match (PieceBorders up1 right1 bottom1 left1) (PieceBorders up2 right2 bottom2 left2) = 
        let 
            testedCorners = zip allBorderNames $ map (uncurry (==)) $ zip [up1, right1, bottom1, left1] [bottom2, left2, up2, right2] 
        in
            map fst $ filter ((==True) . snd) testedCorners 

matchInAllDirections :: PieceBorders -> PieceBorders -> [([BorderName], RotateFuncs)]
matchInAllDirections piece1 piece2 = 
    let matches = map (\f -> let rotatedPiece2 = rotateBorders f piece2 in (match piece1 rotatedPiece2, f)) allPossibleRotationFunc in
        filter (\(m, _) -> not $ null m) matches

matchPieceWithOthers :: Map.Map Int Piece -> (Int, Piece) -> [(Int, [([BorderName], RotateFuncs)])]
matchPieceWithOthers pieces (pieceId, Piece borders _) = 
    [(pieceId2, matchInAllDirections borders borders2) | (pieceId2, Piece borders2 _) <- Map.assocs pieces, pieceId /= pieceId2]

matchAllPieces :: Eq a => Map.Map Int Piece -> [(a, [(a, [([BorderName], PieceBorders -> PieceBorders)])])]
matchAllPieces pieces = 
    let 
        allMatches = [matchPieceWithOthers pieces piece | piece <- Map.assocs pieces]
        nonEmptyMatches = filter (\(_, ( _, matches)) -> not $ null matches) allMatches
        groupedMatches = groupBy (\x y -> fst x == fst y) nonEmptyMatches
    in
        map (\(s:xs) -> (fst s, map snd (s:xs))) groupedMatches

constructFirstRow puzzle upperLeftCorner =
    upperLeftCorner

main :: IO ()
main = do
    file <- readFile "data/test20.txt"
    putStrLn "Day 20"
    putStrLn "Part 1"
    let puzzle = Map.fromList $ parsePuzzle file
    print $ puzzle 
    let allMatches = matchAllPieces puzzle 
    let corners = filter ((==2) . length . snd) allMatches
    print $ product $ map fst corners 
    putStrLn "Part 2"
    let upperLeftBorderMatch = [RightBorder, BottomBorder]
    let Just upperLeftCorner = find (\(id, [(_, [([s1], _)]), (_, [([s2], _)])]) -> s1 `elem` upperLeftBorderMatch && s2 `elem` upperLeftBorderMatch) corners 
    print upperLeftCorner
    -- print $ constructFirstRow allMatchesList upperLeftCorner 