module Main where

import Data.Char (isDigit, digitToInt)

data Token = Symbol Char | Digit Int deriving (Show)

lexify [] = []
lexify (' ':xs) = lexify xs
lexify (s:xs) 
    | isDigit s = Digit (digitToInt s) : lexify xs
    | otherwise = Symbol s : lexify xs

data Expr = Op Char Expr Expr | Num Int deriving (Show) 

{-
    LL(1) grammar
    expr = term expr'
    op = [+*] term op | epsilon
    term = (expr)
    term = number
-}
parseExprV1 :: [Token] -> (Expr, [Token])
parseExprV1 s = 
    let
        (t, xs1) = parseTerm s
    in
        parseOp t xs1

parseOp leftExpr (Symbol op:xs)
    | op `elem` ['*', '+'] =
        let
            (rightExpr, xs1) = parseTerm xs
            expr = Op op leftExpr rightExpr
            (newExpr, xs2) = parseOp expr xs1 
        in (newExpr, xs2)
parseOp leftExpr s = (leftExpr, s)

parseTerm :: [Token] -> (Expr, [Token])
parseTerm (Symbol '(':xs) = 
    let (e, (Symbol ')':xs1)) = parseExprV1 xs in
        (e, xs1)
parseTerm (Digit x:xs) = (Num x, xs)

{-
    LL(1) grammar
    expr = add mul
    mul = * add mul | epsilon
    add = term add'
    add' = + term add' | epsilon
    term = (expr)
    term = number
-}
parseExprV2 :: [Token] -> (Expr, [Token])
parseExprV2 s = 
    let
        (t, xs1) = parseAdd s
    in
        parseMul t xs1

parseMul leftExpr (Symbol '*':xs) =
    let 
        (rightExpr, xs1) = parseAdd xs
        expr = Op '*' leftExpr rightExpr
        (newExpr, xs2) = parseMul expr xs1
    in (newExpr, xs2)
parseMul leftExpr s = (leftExpr, s)

parseAdd s = 
    let
        (t, xs1) = parseTermV2 s
    in
        parseAdd' t xs1

parseAdd' leftExpr (Symbol '+':xs) =
    let 
        (rightExpr, xs1) = parseTermV2 xs
        expr = Op '+' leftExpr rightExpr
        (newExpr, xs2) = parseAdd' expr xs1
    in (newExpr, xs2)
parseAdd' leftExpr s = (leftExpr, s)

parseTermV2 :: [Token] -> (Expr, [Token])
parseTermV2 (Symbol '(':xs) = 
    let (e, (Symbol ')':xs1)) = parseExprV2 xs in
        (e, xs1)
parseTermV2 (Digit x:xs) = (Num x, xs)

getOp '+' = (+)
getOp '*' = (*)

evaluate (Num x) = x 
evaluate (Op op expr1 expr2) = 
    let o = getOp op in
        o (evaluate expr1) (evaluate expr2)

main :: IO ()
main = do
    file <- readFile "data/test18.txt"
    putStrLn "Day 18"
    putStrLn "Part 1"
    print $ sum $ map (evaluate . fst . parseExprV1 . lexify) $ lines file
    putStrLn "Part 2"
    print $ sum $ map (evaluate . fst . parseExprV2 . lexify) $ lines file




