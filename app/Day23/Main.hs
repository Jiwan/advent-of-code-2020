module Main where

import qualified Data.IntMap as IntMap
import Data.List (find)

inputPartOne = [3, 6, 4, 2, 8, 9, 7, 1, 5]
inputPartOneExample = [3, 8, 9, 1, 2, 5, 4, 6, 7]
inputPartTwo = inputPartOne ++ [10..1000000]

createMap l = IntMap.fromList $ zip l (drop 1 $ cycle l)

takeFromMap :: IntMap.IntMap Int -> Int -> Int -> [(Int, Int)]
takeFromMap _ _ 0 = [] 
takeFromMap l index amount = 
    let Just next = IntMap.lookup index l in
    (index, next) : takeFromMap l next (amount - 1)

moveV2 :: IntMap.IntMap Int -> Int -> Int -> IntMap.IntMap Int
moveV2 m _ 0 = m
moveV2 m index x =
    moveV2 newMap newIndex (x - 1)
    where
        elemsConcerned = takeFromMap m index 4
        movedElems = drop 1 elemsConcerned
        newIndex = snd . last $ movedElems
        skipDisplacedElems = IntMap.insert index newIndex m
        Just insertionPoint = find (\i -> i `notElem` map fst movedElems) ([(index - 1), (index - 2)..1] ++ [(length m), (length m - 1)..index])
        Just insertionPointNext = IntMap.lookup insertionPoint m
        newMap = IntMap.union (IntMap.fromList [(insertionPoint, fst . head $ movedElems), (fst . last $ movedElems, insertionPointNext)]) skipDisplacedElems

main :: IO ()
main = do
    putStrLn "Day 23"
    putStrLn "Part 1"
    let m = createMap inputPartOne
    let newMap = moveV2 m (head inputPartOne) 100
    mapM_ (putStr . show) $ drop 1 $ map fst $ takeFromMap newMap 1 (length newMap)
    putStrLn "Part 2"
    let m2 = createMap inputPartTwo
    let newMap2 = moveV2 m2 (head inputPartTwo) 10000000
    print $ product $ take 2 $ drop 1 $ map fst $ takeFromMap newMap2 1 3