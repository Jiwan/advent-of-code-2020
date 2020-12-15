module Main where

import qualified Text.Parsec as Parsec
import Control.Monad () 
import Data.Either (fromRight)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe

smallestBag = do
    Parsec.space
    Parsec.string "no"
    Parsec.many1 (Parsec.noneOf ".")
    return []

parseBagName :: Parsec.Parsec String () String
parseBagName = do
    name <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.string " bag")
    Parsec.skipMany (Parsec.char 's')
    return name

parseContainedBag = do
    Parsec.space
    digits <- (Parsec.many1 Parsec.digit)
    Parsec.space
    name <- parseBagName
    return (name, read digits :: Int)

parseContainedBags = Parsec.sepBy parseContainedBag (Parsec.char ',')

parseBagLine :: Parsec.Parsec String () (String, [(String, Int)])
parseBagLine = do
    bag <- parseBagName
    Parsec.space
    Parsec.string "contain"
    contained <- Parsec.choice [Parsec.try smallestBag, parseContainedBags]
    return (bag, contained) 

eof = do 
    Parsec.eof
    return ("",[]) 

parseBags = do
    Parsec.sepBy (Parsec.choice [parseBagLine, eof]) (Parsec.string ".\n")

findAllContainers :: [(String, [(String, Int)])] -> Set.Set String -> Set.Set String -> Set.Set String
findAllContainers rules toLook found
    | Set.null toLook = found
    | otherwise = let newToLook = map fst $ filter (\(_, subbags) -> not . Set.null $ Set.intersection toLook $ Set.fromList (map fst subbags)) rules in  
        findAllContainers rules (Set.fromList newToLook) (toLook `Set.union` found) 

computeCapacity rules rootBag =
    case fromJust $ Map.lookup rootBag rules of
        contained -> sum $ map (\(bag, amount) -> amount + amount * computeCapacity rules bag) contained 
        [] -> 1



main :: IO ()
main = do
    file <- readFile "data/test7.txt"
    let rules = fromRight [] $ Parsec.parse parseBags "bags" file
    putStrLn "Day 7"
    putStrLn "Part 1"
    print $ (-1 +) $ length $ findAllContainers rules (Set.fromList ["shiny gold"]) Set.empty
    putStrLn "Part 2"
    print $ computeCapacity (Map.fromList rules) "shiny gold"



