module Main where

readInt :: String -> Int
readInt = read

findAmountOfLoop :: Integral a => a -> a -> Int -> Int
findAmountOfLoop 1 _ n = n
findAmountOfLoop x inverse n = findAmountOfLoop ((x * inverse) `mod` 20201227) inverse (n + 1)

crypt :: (Integral t1, Num t2, Eq t2) => t1 -> t1 -> t2 -> t1
crypt seed _ 0 = seed
crypt seed pubKey n = crypt ((seed * pubKey) `mod` 20201227) pubKey (n - 1) 

--  https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm.
extendedEclideanAlgorithm :: Integral a => a -> a -> (a, a, a)
extendedEclideanAlgorithm x y =
    if x >= y then go (x, y) (1, 0) (0, 1) else go (y, x) (0, 1) (1, 0)
    where
        go (old_r, 0) (old_s, _) (old_t, _) = (old_r, old_s, old_t)
        go (old_r, r) (old_s, s) (old_t, t) = 
            let quotient = old_r `quot` r in 
                go (r, old_r - r * quotient) (s, old_s - s * quotient) (t, old_t - t * quotient)

main :: IO ()
main = do
    file <- readFile "data/test25.txt"
    putStrLn "Day 25"
    putStrLn "Part 1"
    let [cardPubKey, doorPubKey] = map readInt . lines $ file
    let (gcd, coefx, coefy) = extendedEclideanAlgorithm 7 20201227  
    -- Ensure that 20201227 7 are co-primes: https://crypto.stanford.edu/pbc/notes/numbertheory/arith.html
    -- Therefore, their gcd should be == 1
    print $ gcd == 1
    -- In that scenario the bezout coeficient of 7 is the multiplicative inverse of 7 in modolu 20201227 https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
    -- We can then multiply the cardPubKey by that inverse until we get back to 1.
    let cardAmountOfLoop = findAmountOfLoop cardPubKey coefx 0
    print cardAmountOfLoop
    print $ crypt 1 doorPubKey cardAmountOfLoop