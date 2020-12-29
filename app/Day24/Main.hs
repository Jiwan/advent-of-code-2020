module Main where

import qualified Data.Set as Set

data Direction = W | SW | NW | E | SE | NE deriving (Show)

parseLine ('w':xs) = W : parseLine xs
parseLine ('s':'w':xs) = SW : parseLine xs
parseLine ('n': 'w': xs) = NW : parseLine xs
parseLine ('e':xs) = E : parseLine xs
parseLine ('s':'e':xs) = SE : parseLine xs
parseLine ('n': 'e': xs) = NE : parseLine xs
parseLine [] = []

parseTiles = map parseLine . lines

move (x, y) W = (x - 2, y)
move (x, y) SW = (x - 1, y - 1)
move (x, y) NW = (x - 1, y + 1)
move (x, y) E = (x + 2, y)
move (x, y) SE = (x + 1, y - 1)
move (x, y) NE = (x + 1, y + 1)

computeBlackTiles :: [[Direction]] -> Set.Set (Int, Int)
computeBlackTiles =
    foldl updateBlackTiles Set.empty
    where
        updateBlackTiles set tilePath =  let reachedTile = foldl move (0, 0) tilePath in 
            if Set.member reachedTile set then Set.delete reachedTile set else Set.insert reachedTile set

getNeighboors coords = map (move coords) [W, SW, NW, E, SE, NE] 

count x = length . filter (==x)

spawnToLife world newWorld coord =
    let
        neighboors = getNeighboors coord 
        aliveNeighboor = count True $ map (`Set.member` world) neighboors
        spawn = aliveNeighboor == 2
    in
        if spawn then Set.insert coord newWorld else newWorld

keepAliveRule world newWorld coord = 
    let
        neighboors = getNeighboors coord 
        aliveNeighboor = count True $ map (`Set.member` world) neighboors
        stayAlive = aliveNeighboor >= 1 && aliveNeighboor <= 2
    in
        if stayAlive then Set.insert coord newWorld else newWorld

playWorld world 0 = world 
playWorld world step = 
    let 
        keptAlive = foldl (keepAliveRule world) Set.empty world
        newOnes = foldl (\newWorld cell -> foldl (spawnToLife world) newWorld (getNeighboors cell)) Set.empty world
    in 
        playWorld (Set.union keptAlive newOnes) (step - 1)

main :: IO ()
main = do
    file <- readFile "data/test24.txt"
    putStrLn "Day 24"
    putStrLn "Part 1"
    let tiles = parseTiles file
    let blackTiles = computeBlackTiles tiles
    print $ length blackTiles 
    putStrLn "Part 2"
    print $ length $ playWorld blackTiles 100 