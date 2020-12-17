module Main where

import Data.Maybe

parseRoom = lines

roomDimensions room = (length (head room), length room)

getSlot room (x, y) 
    | x < 0 || y < 0 || y >= length room || x >= length (head room) = Nothing 
    | otherwise = Just $ (room !! y) !! x

getAdjacent room (x, y) = map (getSlot room) [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

updateCell room (x, y) =
    let adjacents = filter (`notElem` [Just '.', Nothing]) $ getAdjacent room (x, y) in
    case getSlot room (x, y) of
        Just '.' -> '.'
        Just 'L' -> if all (== Just 'L') adjacents then '#' else 'L'
        Just '#' -> if length (filter (== Just '#') adjacents) >= 4 then 'L' else '#' 

scan room (x, y) (dirx, diry) =
    go room (x + dirx, y + diry)
    where 
        go room (x', y') =
            case getSlot room (x', y') of
                Just '.' -> go room (x' + dirx, y' + diry)
                x -> x

scanAllDirections room (x, y) = map (scan room (x, y)) [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)] 

updateCell2 room (x, y) =
    let adjacents = filter (`notElem` [Just '.', Nothing]) $ scanAllDirections room (x, y) in
    case getSlot room (x, y) of
        Just '.' -> '.'
        Just 'L' -> if all (== Just 'L') adjacents then '#' else 'L'
        Just '#' -> if length (filter (== Just '#') adjacents) >= 5 then 'L' else '#' 

updateRoom updateFunc room = 
    let (width, height) = roomDimensions room in
    [[ updateFunc room (i, j) | i <- [0..(width - 1)] ] | j <- [0..(height - 1)]]

converge room update = 
    let updatedRoom = update room in 
        if updatedRoom == room then room else converge updatedRoom update

main :: IO ()
main = do
    file <- readFile "data/test11.txt"
    let room = parseRoom file 
    putStrLn "Day 11"
    putStrLn "Part 1"
    let stableRoom = converge room $ updateRoom updateCell
    print $ length . filter (=='#') $ concat stableRoom
    putStrLn "Part 2"
    let stableRoom2 = converge room $ updateRoom updateCell2
    print $ length . filter (=='#') $ concat stableRoom2