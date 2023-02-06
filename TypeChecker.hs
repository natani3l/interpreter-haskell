{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeChecker where

import Lexer

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty
typeof _ BTrue = Just TBool
typeof _ BFalse = Just TBool
typeof _ (Num _) = Just TNum
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TNum, Just TNum) -> Just TNum
  _ -> Nothing
typeof ctx (Sub s1 s2) = case (typeof ctx s1, typeof ctx s2) of
  (Just TNum, Just TNum) -> Just TNum
  _ -> Nothing
typeof ctx (Mul m1 m2) = case (typeof ctx m1, typeof ctx m2) of
  (Just TNum, Just TNum) -> Just TNum
  _ -> Nothing
typeof ctx (Div d1 d2) = case (typeof ctx d1, typeof ctx d2) of
  (Just TNum, Just TNum) -> Just TNum
  _ -> Nothing
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TBool, Just TBool) -> Just TBool
  _ -> Nothing
type ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TBool, Just TBool) -> Just TBool
  _ -> Nothing
type ctc (Not e1) = case (typeof ctx e1) of
  (Just TBool) -> Just TBool
  _ -> Nothing
typeof ctx (If e e1 e2) =
  case typeof ctx e of
    Just TBool -> case (typeof ctx e1, typeof ctx e2) of
      (Just t1, Just t2) ->
        if t1 == t2
          then Just t1
          else Nothing
      _ -> Nothing
    _ -> Nothing
typeof ctx (Var v) = lookup v ctx
typeof ctx (Lam v t1 b) =
  let Just t2 = typeof ((v, t1) : ctx) b
   in Just (TFun t1 t2)
typeof ctx (App t1 t2) = case (typeof ctx t1, typeof ctx t2) of
  (Just (TFun t11 t12), Just t2) ->
    if t11 == t2
      then Just t12
      else Nothing
  _ -> Nothing
typeof ctx (Eq e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just t1, Just t2) ->
    if t1 == t2
      then Just TBool
      else Nothing
  _ -> Nothing
typeof ctx (Paren e) = typeof ctx e

typecheck :: Expr -> Expr
typecheck e = case typeof [] e of
  Just _ -> e
  _ -> error "Type error"