module Main where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)

lexer = makeTokenParser javaStyle


expr :: ParsecT String u () Integer
expr table = buildExpressionParser table term

term =  parens lexer expr 
    <|> natural lexer

tableForWrong = [[binary "*" (*) AssocLeft, binary "/" (div) AssocLeft , binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]]

binary  name fun assoc = Infix (do{ reservedOp lexer name; return fun }) assoc

main :: IO ()
main = do
    file <- readFile "data/test18.txt"
    putStrLn "Day 18"
    putStrLn "Part 1"
    print "bob"
    print $ parse (expr tableForWrong) "test" "1 + 2 * 3 + 4 * 5 + 6" 


