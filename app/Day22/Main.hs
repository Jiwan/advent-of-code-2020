module Main where

import Data.List.Split (splitOneOf, wordsBy, splitOn)
import Data.Char (isSpace)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intersperse, sortBy, find)
import Data.Function (on)

parseFood :: String -> ([String], [String])
parseFood line = 
    let [ingredients, allergens] = splitOn "(contains " line in
        (wordsBy isSpace ingredients, filter (not . null) $ splitOneOf ", )" allergens)

parseFoods :: String -> [([String], [String])]
parseFoods = map parseFood . lines 

createAllergenMap :: [([String], [String])] -> Map.Map String (Set.Set String)
createAllergenMap = foldr addIngredientsForAllergens Map.empty
    where
        addIngredientsForAllergens (ingredients, allergens) map = foldr (addIngredientForAllergen ingredients) map allergens
        addIngredientForAllergen ingredients allergen map = Map.insert allergen (Set.intersection (Set.fromList ingredients) (Map.findWithDefault (Set.fromList ingredients) allergen map)) map

figureOutAllergens :: Map.Map String (Set.Set String) -> [(String, String)]
figureOutAllergens m 
    | Map.null m = []
    | otherwise = 
        (allergen, ingredient):(figureOutAllergens restOfMap)
        where
            Just (allergen, set) = find ((==1) . length . snd) (Map.assocs m)
            ingredient = Set.elemAt 0 set
            removeAllergenMap = Map.delete allergen m
            restOfMap = Map.map (Set.delete ingredient) removeAllergenMap

main :: IO ()
main = do
    file <- readFile "data/test21.txt"
    putStrLn "Day 21"
    putStrLn "Part 1"
    let foods = parseFoods file
    let allergenMap = createAllergenMap foods
    let safeIngredients = Set.difference (Set.fromList $ concat $ map fst foods) (foldl Set.union Set.empty (Map.elems allergenMap))
    let dangerousIngredients = length . filter (flip Set.member safeIngredients) $ (concat $ map fst foods)
    print dangerousIngredients
    putStrLn "Part 2"
    mapM_ putStr $ intersperse "," $ map snd $ sortBy (compare `on` fst) $ figureOutAllergens allergenMap