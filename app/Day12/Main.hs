module Main where

degToRad = (*) (pi/180)  

parseInstructions :: String -> [(Char, Int)]
parseInstructions = map (\line -> (head line, read $ tail line ::Int)) . lines 

processInstruction :: (Int, Int, Double) -> (Char, Int) -> (Int, Int, Double)
processInstruction (x, y, dir) inst =
    case inst of
        ('N', value) -> (x, y + value, dir)
        ('S', value) -> (x, y - value, dir)
        ('E', value) -> (x + value, y, dir)
        ('W', value) -> (x - value, y, dir)
        ('F', value) -> (x + round (cos dir) * value, y + round (sin dir) * value, dir)
        ('R', value) -> (x, y, dir + degToRad (fromIntegral (-value)))
        ('L', value) -> (x, y, dir + degToRad (fromIntegral value))

rotate :: (Integral a, Integral b) => (Int, Int) -> Int -> (a, b)
rotate (x, y) deg =
    let 
        (x', y') = (fromIntegral x, fromIntegral y)
        rad = degToRad $ fromIntegral deg
    in
        (round ((cos rad * x') - (sin rad * y')), round ((sin rad * x') + (cos rad * y')))

processInstruction2 ((posx, posy), (waypointx, waypointy)) inst = 
    case inst of
        ('N', value) -> ((posx, posy), (waypointx, waypointy + value))
        ('S', value) -> ((posx, posy), (waypointx, waypointy - value))
        ('E', value) -> ((posx, posy), (waypointx + value, waypointy))
        ('W', value) ->  ((posx, posy), (waypointx - value, waypointy))
        ('F', value) -> ((posx + waypointx * value, posy + waypointy * value), (waypointx, waypointy))
        ('R', value) -> ((posx, posy), rotate (waypointx, waypointy) (-value))
        ('L', value) -> ((posx, posy), rotate (waypointx, waypointy) value)

main :: IO ()
main = do
    file <- readFile "data/test12.txt"
    putStrLn "Day 12"
    putStrLn "Part 1"
    let instructions = parseInstructions file
    let (x, y, _) = foldl processInstruction (0, 0, 0) instructions
    print $ abs x + abs y
    putStrLn "Part 2"
    let ((x2, y2), _) = foldl processInstruction2 ((0, 0), (10, 1)) instructions
    print $ abs x2 + abs y2