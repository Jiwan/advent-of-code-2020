module Main where

import qualified Text.Parsec as Parsec
import Data.List (tails, isPrefixOf)
import Data.Bits ( Bits(setBit, clearBit, (.&.), (.|.)) ) 
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.Word (Word64)
import Data.Either (fromRight)

data Instruction = Mask Word64 Word64 [Int] | Mem Word64 Word64 | Error deriving (Show)

toWord64 :: Int -> Word64
toWord64 = fromIntegral

readBase2Integer = foldl (\acc x -> acc * 2 + toWord64 (digitToInt x)) 0

maskSuffix = "mask = " 

parseMemInstruction = do
    Parsec.string "mem["
    addr <- (Parsec.many1 Parsec.digit)
    Parsec.string "] = "
    value <- (Parsec.many1 Parsec.digit)
    return (Mem (read addr) (read value))

parseOrMask = readBase2Integer . map (\x -> if x == '1' then '1' else '0')
parseAndMask = readBase2Integer . map (\x -> if x == '0' then '0' else '1')
parseFloatingBits = map fst . filter (('X' ==) .  snd ). zip [0..] . reverse

parseInstruction line
    | isPrefixOf maskSuffix line = 
        let maskPattern = drop (length maskSuffix) line in 
            Mask (parseOrMask maskPattern) (parseAndMask maskPattern) (parseFloatingBits maskPattern)
    | isPrefixOf "mem" line = fromRight Error $ Parsec.parse parseMemInstruction "test" line

parseInstructions = map parseInstruction . lines

executeInstruction (memory, (orMask, andMask)) instruction =
    case instruction of 
        Mask or and _ -> (memory, (or, and))
        Mem addr value -> let actualValue = (value .|. orMask) .&. andMask in (Map.insert addr actualValue memory, (orMask, andMask))

combinations l =
    go l [[]]
    where
        go [] comb = comb
        go (s:xs) comb = let newcomb = map ((:) s) comb in go xs (comb ++ newcomb)

executeInstruction2 :: (Map.Map Word64 Word64, (Word64, Word64, [Int])) -> Instruction -> (Map.Map Word64 Word64, (Word64, Word64, [Int]))
executeInstruction2 (memory, (orMask, andMask, floatingBits)) instruction =
    case instruction of 
        Mask or and floats -> (memory, (or, and, floats))
        Mem addr value -> 
            let
                orAddr =  (addr .|. orMask) 
                baseAddr = foldr (flip clearBit) orAddr floatingBits
                applyCombination bits m =  Map.insert (foldr (flip setBit) baseAddr bits) value m
                updatedMemory = foldr applyCombination memory (combinations floatingBits)
            in
                (updatedMemory, (orMask, andMask, floatingBits))

main :: IO ()
main = do
    file <- readFile "data/test14.txt"
    putStrLn "Day 14"
    putStrLn "Part 1"
    let instructions = parseInstructions file
    let resultState = foldl executeInstruction (Map.empty, (0, 0)) instructions
    print $ sum $ Map.elems $ fst resultState
    putStrLn "Part 2"
    let resultState2 = foldl executeInstruction2 (Map.empty, (0, 0, [])) instructions
    print $ sum $ Map.elems $ fst resultState2