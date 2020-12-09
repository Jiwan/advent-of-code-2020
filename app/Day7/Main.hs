module Main where

import qualified Text.Parsec as Parsec
import Control.Monad 

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

containedBag = do
    Parsec.space
    digits <- (Parsec.many1 Parsec.digit)
    Parsec.space
    name <- parseBagName
    return (name, read digits :: Int)

containedBags = Parsec.sepBy containedBag (Parsec.char ',')

bagLine :: Parsec.Parsec String () (String, [(String, Int)])
bagLine = do
    bag <- parseBagName
    Parsec.space
    Parsec.string "contain"
    contained <- Parsec.choice [Parsec.try smallestBag, containedBags]
    return (bag, contained) 

eof = do 
    Parsec.eof
    return ("",[]) 

bags = do
    Parsec.sepBy (Parsec.choice [bagLine, eof]) (Parsec.string ".\n")

main :: IO ()
main = do
    putStrLn "Day 7"
    file <- readFile "data/test7.txt"
    print $ Parsec.parse bags "test" file
