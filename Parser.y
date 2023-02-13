{
module Parser where 

import Lexer 
}

%name parser 
%tokentype { Token }
%error { parseError }

%token
    num         { TokenNum $$ }
    '+'         { TokenAdd }
    '-'         { TokenSub }
    '*'         { TokenMul }
    '/'         { TokenDiv }
    '!'         { TokenNot }
    '>'         { TokenGtr }
    '>='        { TokenGrOrEq }
    '<'         { TokenLess }
    "&&"        { TokenAnd }
    "||"        { TokenOr }
    "=="        { TokenEq }
    let         { TokenLet }
    in          { TokenIn }
    '='         { TokenAtt }
    true        { TokenTrue }
    false       { TokenFalse }
    if          { TokenIf }
    then        { TokenThen }
    else        { TokenElse }
    var         { TokenVar $$ }
    '\\'        { TokenLam }
    ':'         { TokenColon }
    ','         { TokenComma }
    "->"        { TokenArrow }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    Bool        { TokenBoolean }
    Number      { TokenNumber }

%nonassoc if then else
%left '+' '-'
%left '*' '/'
%left "&&" "||"
%right '!'
%left "==" ">=" ">" "<"

%% 

Exp     : num                        { Num $1 }
        | var                        { Var $1 }
        | false                      { BFalse }
        | true                       { BTrue }
        | Exp '+' Exp                { Add $1 $3 }
        | Exp '-' Exp                { Sub $1 $3 }
        | Exp '*' Exp                { Mul $1 $3 }
        | Exp '/' Exp                { Div $1 $3 }
        | '!' Exp                    { Not $2 }
        | Exp "&&" Exp               { And $1 $3 }
        | Exp "||" Exp               { Or $1 $3 }
        | Exp '>=' Exp               { GrOrEq $1 $3}
        | Exp '>'  Exp               { Gtr $1 $3}
        | Exp '<'  Exp               { Less $1 $3}
        | if Exp then Exp else Exp   { If $2 $4 $6 }
        | '\\' var ':' Type "->" Exp { Lam $2 $4 $6 }
        | let var '=' Exp in Exp     { Let $2 $4 $6 }
        | Exp Exp                    { App $1 $2 }
        | '(' Exp ')'                { Paren $2 }
        | Exp "==" Exp               { Eq $1 $3 }
        | '(' Exp ',' Exp ')'        { Tup $2 $4 }

Type    : Bool                       { TBool }
        | Number                     { TNum }
        | '(' Type ',' Type ')'      { TTup $2 $4 }
        | '(' Type "->" Type ')'     { TFun $2 $4 }


{ 

parseError :: [Token] -> a 
parseError _ = error "Syntax error!"

}
