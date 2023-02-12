module Main where

import Interpreter (eval)
import Lexer (lexer)
import Parser (parser)
import TypeChecker (typecheck)

main :: IO ()
main = getContents >>= print . eval . typecheck . parser . lexer
-- main = getContents >>= print . typecheck . parser . lexer