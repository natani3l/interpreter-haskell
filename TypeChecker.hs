{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeChecker where

import Interpreter
import Lexer

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty
typeof _ BTrue = Just TBool
typeof _ BFalse = Just TBool
typeof _ (Num _) = Just TNum
typeof ctx (Pair t1 t2) = case (typeof ctx t1, typeof ctx t2) of
  (Just e1, Just e2) -> Just (TPair e1 e2)
  _ -> Nothing
typeof ctx (Snd v) = case typeof ctx v of
  Just (TPair _ t2) -> Just t2
  _ -> Nothing

typeof ctx (Fst v) = case typeof ctx v of
  Just (TPair t1 _) -> Just t1
  _ -> Nothing


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
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TBool, Just TBool) -> Just TBool
  _ -> Nothing
typeof ctx (Not e1) = case (typeof ctx e1) of
  (Just TBool) -> Just TBool
  _ -> Nothing
typeof ctx (Let v1 v2 v3) = case (typeof ctx v2, typeof ctx $ subst v1 v2 v3) of
  (Just e1, Just e2) ->
    if e1 == e2
      then Just e1
      else Nothing
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
typeof ctx (Less l1 l2) = case (typeof ctx l1, typeof ctx l2) of
  (Just TNum, Just TNum) -> Just TNum
  (Just TBool, Just TBool) -> Just TBool
  _ -> Nothing
typeof ctx (Gtr g1 g2) = case (typeof ctx g1, typeof ctx g2) of
  (Just TNum, Just TNum) -> Just TNum
  _ -> Nothing
typeof ctx (GrOrEq g1 g2) = case (typeof ctx g1, typeof ctx g2) of
  (Just TNum, Just TNum) -> Just TNum
  _ -> Nothing
typeof ctx (Paren e) = typeof ctx e

typecheck :: Expr -> Expr
typecheck e = case typeof [] e of
  Just _ -> e
  _ -> error "Type error"