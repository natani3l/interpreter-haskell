{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Interpreter where

import Data.Char (isDigit)
import Lexer

subst :: String -> Expr -> Expr -> Expr
subst x n b@(Var v) =
  if v == x
    then n
    else b
subst x n (Lam v t b) = Lam v t (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (Mul m1 m2) = Mul (subst x n m1) (subst x n m2)
subst x n (Sub s1 s2) = Sub (subst x n s1) (subst x n s2)
subst x n (Div d1 d2) = Div (subst x n d1) (subst x n d2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (Not e1) = Not (subst x n e1)
subst x n (Or e1 e2) = Or (subst x n e1) (subst x n e2)
subst x n (If e e1 e2) = If (subst x n e) (subst x n e1) (subst x n e2)
subst x n (Paren e) = Paren (subst x n e)
subst x n (Eq e1 e2) = Eq (subst x n e1) (subst x n e2)
subst x n (Gtr e1 e2) = Gtr (subst x n e1) (subst x n e2)
subst x n (GrOrEq e1 e2) = GrOrEq (subst x n e1) (subst x n e2)
subst x n (Less e1 e2) = Less (subst x n e1) (subst x n e2)
subst x n (Let v1 v2 v3) = Let v1 (subst x n v2) (subst x n v3)
subst x n e = e

isValue :: Expr -> Bool
isValue BTrue = True
isValue BFalse = True
isValue (Num _) = True
isValue (Pair t1 t2) = isValue t1 && isValue t2
isValue Lam {} = True
isValue _ = False

isNumber :: Expr -> Int
isNumber (Num n) = n
isNumber BTrue = 1
isNumber BFalse = 0

isPair :: Expr -> (Expr, Expr)
isPair (Pair t1 t2) = (t1, t2)

stepAdd :: Expr -> Maybe Expr
stepAdd (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
stepAdd (Add (Num n1) e2) = case step e2 of
  Just e2' -> Just (Add (Num n1) e2')
  _ -> Nothing
stepAdd (Add e1 e2) = case step e1 of
  Just e1' -> Just (Add e1' e2)
  _ -> Nothing

stepSub :: Expr -> Maybe Expr
stepSub (Sub (Num s1) (Num s2)) = Just (Num (s1 - s2))
stepSub (Sub (Num s1) s2) = case step s2 of
  Just s2' -> Just (Sub (Num s1) s2')
  _ -> Nothing
stepSub (Sub s1 s2) = case step s1 of
  Just s1' -> Just (Sub s1' s2)
  _ -> Nothing

stepMul :: Expr -> Maybe Expr
stepMul (Mul (Num m1) (Num m2)) = Just (Num (m1 * m2))
stepMul (Mul (Num m1) m2) = case step m2 of
  Just m2' -> Just (Mul (Num m1) m2')
  _ -> Nothing
stepMul (Mul m1 m2) = case step m1 of
  Just m1' -> Just (Mul m1' m2)
  _ -> Nothing

stepDiv :: Expr -> Maybe Expr
stepDiv (Div (Num d1) (Num d2)) = Just (Num (d1 `div` d2))
stepDiv (Div (Num d1) d2) = case step d2 of
  Just d2' -> Just (Div (Num d1) d2')
  _ -> Nothing
stepDiv (Div d1 d2) = case step d1 of
  Just d1' -> Just (Div d1' d2)
  _ -> Nothing

stepAnd :: Expr -> Maybe Expr
stepAnd (And BTrue e2) = Just e2
stepAnd (And BFalse _) = Just BFalse
stepAnd (And e1 e2) = case step e1 of
  Just e1' -> Just (And e1' e2)
  _ -> Nothing

stepOr :: Expr -> Maybe Expr
stepOr (Or BFalse BFalse) = Just BFalse
stepOr (Or e1 BFalse) = Just e1
stepOr (Or BFalse e2) = Just e2
stepOr (Or e1 e2) = case step e1 of
  Just e1' -> Just (Or e1' e2)
  _ -> Nothing

step :: Expr -> Maybe Expr
step (Add n1 n2) = stepAdd (Add n1 n2)
step (Sub s1 s2) = stepSub (Sub s1 s2)
step (Mul m1 m2) = stepMul (Mul m1 m2)
step (Div d1 d2) = stepDiv (Div d1 d2)
step (And a1 a2) = stepAnd (And a1 a2)
step (Or o1 o2) = stepOr (Or o1 o2)
step (Not BTrue) = Just BFalse
step (Not BFalse) = Just BTrue
step (Not e1) = case step e1 of
  Just e1' -> Just (Not e1')
  _ -> Nothing
step (Let v1 v2 v3) = Just (subst v1 v2 v3)  
step (If BTrue e1 _) = Just e1
step (If BFalse _ e2) = Just e2
step (If e e1 e2) = case step e of
  Just e' -> Just (If e' e1 e2)
  _ -> Nothing
step (App e1@(Lam x t b) e2)
  | isValue e2 = Just (subst x e2 b)
  | otherwise = case step e2 of
    Just e2' -> Just (App e1 e2')
    _ -> Nothing
step (App e1 e2) = case step e1 of
  Just e1' -> Just (App e1' e2)
  _ -> Nothing
step (Paren e) = Just e
step (Eq e1 e2)
  | isValue e1 && isValue e2 =
    if e1 == e2
      then Just BTrue
      else Just BFalse
  | isValue e1 = case step e2 of
    Just e2' -> Just (Eq e1 e2')
    _ -> Nothing
  | otherwise = case step e1 of
    Just e1' -> Just (Eq e1' e2)
    _ -> Nothing
step (Less l1 l2)
  | isValue l1 && isValue l2 =
    if isNumber (l1) < isNumber (l2) -- Need to fix this 🙃
      then Just BTrue
      else Just BFalse
  | isValue l1 = case step l2 of
    Just l2' -> Just (Less l1 l2')
    _ -> Nothing
  | otherwise = case step l1 of
    Just l1' -> Just (Less l1' l2)
    _ -> Nothing
step (Gtr g1 g2)
  | isValue g1 && isValue g2 =
    if isNumber (g1) > isNumber (g2)
      then Just BTrue
      else Just BFalse
  | isValue g1 = case step g2 of
    Just g2' -> Just (Gtr g1 g2')
    _ -> Nothing
  | otherwise = case step g1 of
    Just g1' -> Just (Gtr g1' g2)
    _ -> Nothing
step (GrOrEq g1 g2)
  | isValue g1 && isValue g2 =
    if isNumber (g1) >= isNumber (g2)
      then Just BTrue
      else Just BFalse
  | isValue g1 = case step g2 of
    Just g2' -> Just (GrOrEq g1 g2')
    _ -> Nothing
  | otherwise = case step g1 of
    Just g1' -> Just (GrOrEq g1' g2)
    _ -> Nothing

step (Pair t1 t2)
  | isValue t1 && isValue t2 = Just (Pair t1 t2)
  | isValue t1 = case step t2 of
    Just t2' -> Just (Pair t1 t2')
    _ -> Nothing
  | otherwise = case step t1 of
    Just t1' -> Just (Pair t1' t2)
    _ -> Nothing
    
step (Fst (Pair t1 t2)) = Just t1
step (Fst e) = case step e of
  Just e' -> Just (Fst e')
  _ -> Nothing

step (Snd (Pair t1 t2)) = Just t2
step (Snd e) = case step e of
  Just e' -> Just (Snd e')
  _ -> Nothing
  
step e = Just e





eval :: Expr -> Expr
eval e
  | isValue e = e
  | otherwise = case step e of
    Just e' -> eval e'
    _ -> error "Interpreter error!"
