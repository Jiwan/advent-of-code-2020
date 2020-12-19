module Main where

import qualified Data.Set as Set

getNeighboors (x, y, z) =  drop 1 [(x + i, y + j, z + k) | i <- [0, -1, 1], j <- [0, -1, 1], k <- [0, -1, 1]]
getNeighboors4d (x, y, z, w) =  drop 1 [(x + i, y + j, z + k, w + h) | i <- [0, -1, 1], j <- [0, -1, 1], k <- [0, -1, 1], h <- [0, -1, 1]]

parseLine state (x, line) = foldl (\s (y, c) -> if c == '#' then Set.insert (x, y, 0) s else s) state $ zip [0..] line 

parseWorld :: String -> Set.Set (Int, Int, Int)
parseWorld = foldl parseLine Set.empty . zip [0..] . lines

count x = length . filter (==x)

spawnToLife getNeighboorsFunc world newWorld coord =
    let
        neighboors = getNeighboorsFunc coord 
        aliveNeighboor = count True $ map (`Set.member` world) neighboors
        spawn = aliveNeighboor == 3
    in
        if spawn then Set.insert coord newWorld else newWorld

keepAliveRule getNeighboorsFunc world newWorld coord = 
    let
        neighboors = getNeighboorsFunc coord 
        aliveNeighboor = count True $ map (`Set.member` world) neighboors
        stayAlive = aliveNeighboor >= 2 && aliveNeighboor <= 3
    in
        if stayAlive then Set.insert coord newWorld else newWorld

playWorld :: (Ord a) => (a -> [a]) -> Set.Set a -> Int -> Set.Set a 
playWorld getNeighboorsFunc world 0 = world 
playWorld getNeighboorsFunc world step = 
    let 
        keptAlive = foldl (keepAliveRule getNeighboorsFunc world) Set.empty world
        newOnes = foldl (\newWorld cell -> foldl (spawnToLife getNeighboorsFunc world) newWorld (getNeighboorsFunc cell)) Set.empty world
    in 
        playWorld getNeighboorsFunc (Set.union keptAlive newOnes) (step -1)

main :: IO ()
main = do
    file <- readFile "data/test17.txt"
    putStrLn "Day 17"
    putStrLn "Part 1"
    let world = parseWorld file
    print $ length $ playWorld getNeighboors world 6
    putStrLn "Part 2"
    let world4d = Set.map (\(x, y, z) -> (x, y, z, 0)) world 
    print $ length $ playWorld getNeighboors4d world4d 6

