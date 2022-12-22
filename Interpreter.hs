{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Interpreter where

import Lexer

subst :: String -> Expr -> Expr -> Expr
subst x n b@(Var v) =
  if v == x
    then n
    else b
subst x n (Lam v t b) = Lam v t (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (If e e1 e2) = If (subst x n e) (subst x n e1) (subst x n e2)
subst x n (Paren e) = Paren (subst x n e)
subst x n (Eq e1 e2) = Eq (subst x n e1) (subst x n e2)
subst x n e = e

isvalue :: Expr -> Bool
isvalue BTrue = True
isvalue BFalse = True
isvalue (Num _) = True
isvalue Lam {} = True
isvalue _ = False

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
  Just s2' -> Just (Sub (Num s1) s2)
  _ -> Nothing
stepSub (Sub s1 s2) = case step s1 of
  Just s1' -> Just (Sub s1' s2)
  _ -> Nothing

stepMul :: Expr -> Maybe Expr
stepMul (Mul (Num m1) (Num m2)) = Just (Num (m1 * m2))
stepMul (Mul (Num m1) m2) = case step m2 of
  Just m2' -> Just (Mul (Num m1) m2)
  _ -> Nothing
stepMul (Mul m1 m2) = case step m1 of
  Just m1' -> Just (Mul m1' m2)
  _ -> Nothing

stepDiv :: Expr -> Maybe Expr
stepDiv (Div (Num d1) (Num d2)) = Just (Num (d1 `div` d2))
stepDiv (Div (Num d1) d2) = case step d2 of
  Just d2' -> Just (Div (Num d1) d2)
  _ -> Nothing
stepDiv (Div d1 d2) = case step d1 of
  Just d1' -> Just (Div d1' d2)
  _ -> Nothing

step :: Expr -> Maybe Expr
step (Add n1 n2) = stepAdd (Add n1 n2)
step (Sub s1 s2) = stepSub (Sub s1 s2)
step (Mul m1 m2) = stepMul (Mul m1 m2)
step (Div d1 d2) = stepDiv (Div d1 d2)
step (And BTrue e2) = Just e2
step (And BFalse _) = Just BFalse
step (And e1 e2) = case step e1 of
  Just e1' -> Just (And e1' e2)
  _ -> Nothing
step (If BTrue e1 _) = Just e1
step (If BFalse _ e2) = Just e2
step (If e e1 e2) = case step e of
  Just e' -> Just (If e' e1 e2)
  _ -> Nothing
step (App e1@(Lam x t b) e2)
  | isvalue e2 = Just (subst x e2 b)
  | otherwise = case step e2 of
    Just e2' -> Just (App e1 e2')
    _ -> Nothing
step (App e1 e2) = case step e1 of
  Just e1' -> Just (App e1' e2)
  _ -> Nothing
step (Paren e) = Just e
step (Eq e1 e2)
  | isvalue e1 && isvalue e2 =
    if e1 == e2
      then Just BTrue
      else Just BFalse
  | isvalue e1 = case step e2 of
    Just e2' -> Just (Eq e1 e2')
    _ -> Nothing
  | otherwise = case step e1 of
    Just e1' -> Just (Eq e1' e2)
    _ -> Nothing
step e = Just e

eval :: Expr -> Expr
eval e
  | isvalue e = e
  | otherwise = case step e of
    Just e' -> eval e'
    _ -> error "Interpreter error!"
