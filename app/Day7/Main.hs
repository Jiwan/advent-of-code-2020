module Main where

import qualified Text.Parsec as Parsec
import Control.Monad () 
import Data.Either (fromRight)
import qualified Data.Set as Set

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

main :: IO ()
main = do
    putStrLn "Day 7"
    file <- readFile "data/test7.txt"
    let rules = fromRight [] $ Parsec.parse parseBags "bags" file
    print $ (-1 +) $ length $ findAllContainers rules (Set.fromList ["shiny gold"]) Set.empty

