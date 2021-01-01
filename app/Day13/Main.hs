module Main where

import Data.List.Split (splitOn)
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Data.List (sortBy, subsequences)
import Data.Function (on)

parseData file = 
    let 
        l = lines file
        departTime =  read $ head l :: Int
        buses =  map (\(x, y) ->(-x, read y ::Int)) . filter ((/="x") . snd) $ zip [0..] $ splitOn "," $ l !! 1
    in
        (departTime, buses)

--  https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm.
extendedEclideanAlgorithm :: Integral a => a -> a -> (a, a, a)
extendedEclideanAlgorithm x y =
    if x >= y then go (x, y) (1, 0) (0, 1) else go (y, x) (0, 1) (1, 0)
    where
        go (old_r, 0) (old_s, _) (old_t, _) = (old_r, old_s, old_t)
        go (old_r, r) (old_s, s) (old_t, t) = 
            let quotient = old_r `quot` r in 
                go (r, old_r - r * quotient) (s, old_s - s * quotient) (t, old_t - t * quotient)

invmod :: Integral a => a -> a -> a
invmod x y = let (_, coefx, _) = extendedEclideanAlgorithm x y in coefx `mod` y

combinations k ns = filter ((k==).length) $ subsequences ns

main :: IO ()
main = do
    file <- readFile "data/test13.txt"
    putStrLn "Day 13"
    putStrLn "Part 1"
    let (departTime, buses) = parseData file
    print buses
    let (bus, time) = minimumBy (comparing snd) $ map (\x -> let m = departTime `mod` x in (x, (x - m))) (map snd buses)
    print $ bus * time
    putStrLn "Part 2"
    -- The idea is to find a time t which is expressed a couple of congruences (negative departure time modulo bus ID).
    -- We are going to use the chinese remainder theorem to figure out the time: https://shainer.github.io/crypto/math/2017/10/22/chinese-remainder-theorem.html
    -- Note, we are going to use the Gauss algorithm, but one could also use a search by sieving: https://en.wikipedia.org/wiki/Chinese_remainder_theorem
    -- First we need to ensure that all bus ID are coprime pairwise.
    print $ all (\(gcd, _, _) -> gcd == 1) $ map (\[x, y] -> extendedEclideanAlgorithm x y) $ combinations 2  (map snd buses)
    -- Then we need to find the product of all modulo, our buses id here...
    let moduliProduct = product $ map snd buses
    -- Then we can find all the 
    let res = foldl (\acc (t, busId) -> let b = moduliProduct `div` busId in acc + (t * b * invmod b busId)) 0 buses
    print $ res `mod` moduliProduct