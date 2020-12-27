module Main where

import Data.List.Split (splitOn)
import Data.Word (Word16)
import Data.Bits (Bits(setBit, testBit))
import Data.Foldable (find)
import Data.Maybe
import Data.List (transpose, groupBy)
import qualified Data.Map as Map

data BorderName = UpBorder | RightBorder | BottomBorder | LeftBorder deriving (Show, Eq)

allBorderNames = [UpBorder, RightBorder, BottomBorder, LeftBorder]

data PieceBorders = PieceBorders Word16 Word16 Word16 Word16 deriving (Show)

data Piece = Piece { pieceId :: Int, borders :: PieceBorders, shape :: [String] } deriving (Show)

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

rotateContentRight = transpose . reverse

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
        (pieceNumber, Piece pieceNumber (PieceBorders up right bottom left) (map (init . tail) $ init $ tail shape))

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

matchPieceWithOthers :: Map.Map Int Piece -> Piece -> [(Int, [([BorderName], RotateFuncs)])]
matchPieceWithOthers pieces (Piece pieceId borders _) = 
    let
        matches = [(pieceId2, matchInAllDirections borders borders2) | Piece pieceId2 borders2 _ <- Map.elems pieces, pieceId /= pieceId2]
    in
        filter (not . null . snd) matches

matchAllPieces :: Map.Map Int Piece -> [(Int, [(Int, [([BorderName], RotateFuncs)])])]
matchAllPieces pieces = 
    [(pieceId piece, matchPieceWithOthers pieces piece) | piece <- Map.elems pieces]

findPieceMatching :: Map.Map Int Piece -> Piece -> BorderName -> Maybe Piece 
findPieceMatching pieces piece border = 
    let
        matchingPieces = matchPieceWithOthers pieces piece 
        matchingBorders = map (\(id, matches) -> (id, filter (\(borders, _) -> border `elem` borders) matches)) matchingPieces
        filtered = map (\(id, borders) -> (id, map snd borders)) $ filter (not . null . snd) matchingBorders
    in 
        case filtered of
            [] -> Nothing 
            [(pieceId, [rotateFuncs])] -> 
                let
                    Just (Piece id borders shape) = Map.lookup pieceId pieces
                    newPiece = Piece id (rotateBorders rotateFuncs borders) (rotateContent rotateFuncs shape)
                in
                    Just newPiece 
                
constructFirstRow :: Map.Map Int Piece -> [Piece] -> ([Piece], Map.Map Int Piece)
constructFirstRow pieces row =
    let
        rightMostPiece = last row 
    in
        case findPieceMatching pieces rightMostPiece RightBorder of
            Just piece -> constructFirstRow (Map.delete (pieceId piece) pieces) (row ++ [piece])
            _ -> (row, pieces) 

constructRowsBellow :: Map.Map Int Piece -> [[Piece]] -> [[Piece]]
constructRowsBellow pieces rows 
    | Map.null pieces = rows
    | otherwise =
        let
            lastRow = last rows
            newRow = mapMaybe (\piece -> findPieceMatching pieces piece BottomBorder) lastRow
            remainingPieces = foldl (\acc k -> Map.delete (pieceId k) acc) pieces newRow
        in
           constructRowsBellow remainingPieces $ rows ++ [newRow]


renderPuzzle :: [[Piece]] -> [String]
renderPuzzle = concatMap (map concat . transpose . map shape)

seaMonster = [
    "                  # ", 
    "#    ##    ##    ###", 
    " #  #  #  #  #  #   "]

findMonster :: [String] -> [String] -> [(Int, Int)]
findMonster puzzle monster =
    let
       monsterWidth = length $ head monster
       monsterHeight = length monster
       puzzleWidth = length $ head puzzle
       puzzleHeight = length puzzle
       points = [(x, y) | y <- [0..(puzzleHeight - monsterHeight - 1)], x <- [0..(puzzleWidth - monsterWidth - 1)]]
       slice (x, y) = map (take monsterWidth . drop x) (take monsterHeight (drop y puzzle))
       compare (c1, c2) = (c2 == ' ') || (c1 == c2) 
       testPointFunc point = all (==True) $ zipWith (curry compare) (concat $ slice point) (concat monster) 
    in
        map snd $ filter ((==True) . fst) $ map (\p -> (testPointFunc p, p)) points

update :: Int -> a -> [a] -> [a]
update n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs

highlightMonster :: [String] -> [(Int, Int)] -> [String] -> [String]
highlightMonster puzzle points monster = 
    foldr updateAtPoint puzzle points
    where
        monsterWidth = length $ head monster
        monsterHeight = length monster
        updatePuzzle (x, y) p = let updatedRow = update x 'O' (p !! y) in update y updatedRow p
        monsterDots (monsterPosX, monsterPosY) = [(x + monsterPosX, y + monsterPosY) |  x <- [0..(monsterWidth - 1)], y <- [0..(monsterHeight - 1)], ((monster !! y) !! x) == '#']
        updateAtPoint point p = foldr updatePuzzle p (monsterDots point)

main :: IO ()
main = do
    file <- readFile "data/test20.txt"
    putStrLn "Day 20"
    putStrLn "Part 1"
    let pieces = Map.fromList $ parsePuzzle file
    let allMatches = matchAllPieces pieces 
    let corners = filter ((==2) . length . snd) allMatches
    print $ product $ map fst corners 
    putStrLn "Part 2"
    let upperLeftBorderMatch = [RightBorder, BottomBorder]
    let Just (upperLeftCornerId, _) = find (\(_, [(_, [([s1], _)]), (_, [([s2], _)])]) -> s1 `elem` upperLeftBorderMatch && s2 `elem` upperLeftBorderMatch) corners 
    let Just upperLeftCorner = Map.lookup upperLeftCornerId pieces
    let (firstRow, remainingPieces) = constructFirstRow (Map.delete upperLeftCornerId pieces) [upperLeftCorner] 
    let solvedPuzzle = renderPuzzle $ constructRowsBellow remainingPieces [firstRow]
    let rotatedPuzzlesAndMonsters = map (\(RotateFuncs _ f) -> let rotatedPuzzle = f solvedPuzzle in (findMonster rotatedPuzzle seaMonster, rotatedPuzzle)) allPossibleRotationFunc
    let Just (monsterLocs, rotatedPuzzle) = find (not . null . fst) rotatedPuzzlesAndMonsters 
    let puzzleWithHighlightedMonster = highlightMonster rotatedPuzzle monsterLocs seaMonster
    mapM_ print $ puzzleWithHighlightedMonster
    print $ length . filter (=='#') $ concat puzzleWithHighlightedMonster 