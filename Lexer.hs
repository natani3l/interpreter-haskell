module Lexer where

import Data.Char

data Ty
  = TBool
  | TNum
  | TFun Ty Ty
  deriving (Show, Eq)

data Expr
  = BTrue
  | BFalse
  | Num Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | And Expr Expr
  | Not Expr
  | Or Expr Expr
  | If Expr Expr Expr
  | Var String
  | Lam String Ty Expr
  | App Expr Expr
  | Paren Expr
  | Eq Expr Expr
  | Gtr Expr Expr
  | GrOrEq Expr Expr
  | Less Expr Expr
  | Let String Expr Expr
  deriving (Show, Eq)

data Token
  = TokenTrue
  | TokenFalse
  | TokenNum Int
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenDiv
  | TokenNot
  | TokenAnd
  | TokenOr
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenVar String
  | TokenLam
  | TokenColon
  | TokenArrow
  | TokenLParen
  | TokenRParen
  | TokenBoolean
  | TokenNumber
  | TokenEq
  | TokenGtr
  | TokenGrOrEq
  | TokenLess
  | TokenLet
  | TokenAtt
  | TokenIn
  deriving (Show)

isToken :: Char -> Bool
isToken c = elem c "->&|=<!"

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : cs) = TokenAdd : lexer cs
lexer ('-' : '>' : cs) = TokenArrow : lexer cs
lexer ('-' : cs) = TokenSub : lexer cs
lexer ('*' : cs) = TokenMul : lexer cs
lexer ('/' : cs) = TokenDiv : lexer cs
lexer ('\\' : cs) = TokenLam : lexer cs
lexer (':' : cs) = TokenColon : lexer cs
lexer ('(' : cs) = TokenLParen : lexer cs
lexer (')' : cs) = TokenRParen : lexer cs
lexer (c : cs)
  | isSpace c = lexer cs
  | isDigit c = lexNum (c : cs)
  | isAlpha c = lexKW (c : cs)
  | isToken c = lexSymbol (c : cs)
lexer _ = error "Lexical error: caracter inválido!"

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of
  (num, rest) -> TokenNum (read num) : lexer rest

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of
  ("true", rest) -> TokenTrue : lexer rest
  ("false", rest) -> TokenFalse : lexer rest
  ("if", rest) -> TokenIf : lexer rest
  ("then", rest) -> TokenThen : lexer rest
  ("else", rest) -> TokenElse : lexer rest
  ("Bool", rest) -> TokenBoolean : lexer rest
  ("Number", rest) -> TokenNumber : lexer rest
  ("let", rest) -> TokenLet : lexer rest
  ("in", rest) -> TokenIn : lexer rest
  (var, rest) -> TokenVar var : lexer rest

lexSymbol :: String -> [Token]
lexSymbol cs = case span isToken cs of
  ("->", rest) -> TokenArrow : lexer rest
  ("!", rest) -> TokenNot : lexer rest
  ("&&", rest) -> TokenAnd : lexer rest
  ("||", rest) -> TokenOr : lexer rest
  ("==", rest) -> TokenEq : lexer rest
  (">", rest) -> TokenGtr : lexer rest
  (">=", rest) -> TokenGrOrEq : lexer rest
  ("<", rest) -> TokenLess : lexer rest
  ("=", rest) -> TokenAtt : lexer rest
  _ -> error "Lexical error: símbolo inválido!"