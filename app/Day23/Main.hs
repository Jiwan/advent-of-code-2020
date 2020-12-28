module Main where

import Data.List (elemIndex)
import Debug.Trace (trace)

input = [3, 6, 4, 2, 8, 9, 7, 1, 5]
input2 = [3, 8, 9, 1, 2, 5, 4, 6, 7]

findInsertPlace l 0  = findInsertPlace l 9
findInsertPlace l dest = case elemIndex dest l of
    Just i -> i
    _ -> findInsertPlace l (dest - 1) 

move l _ 0 = l 
move l index x =
    trace (show (l, index, x)) move newList ((index + 1) `mod` 9) (x - 1)
    where  
        infiniteList = concat $ repeat l
        currentElem = l !! index
        elemsToInsert = take 3 $ drop (index + 1) infiniteList 
        rest = take 5 $ drop (index + 4) infiniteList
        insertPlace = findInsertPlace rest (currentElem - 1)
        newList = currentElem : (take (insertPlace + 1) rest) ++ elemsToInsert ++ (drop (insertPlace + 1) rest)


main :: IO ()
main = do
    putStrLn "Day 23"
    putStrLn "Part 1"
    print $ move input2 0 100
