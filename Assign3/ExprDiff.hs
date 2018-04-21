{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module : ExprDiff
Description : Contains a type class and instances for differentiable expressions
Copyright : (c) Seda Mete @2018
License : WTFPL
Maintainer : metes1@mcmaster.ca
Stability : experimental
Portability : POSIX

This module contains the type class 'DiffExpr'. 'DiffExpr' has methods over the 
Expr datatype that assist with the construction and evaluation of differentiable 
expressions.
-}

module ExprDiff where

import ExprType

import qualified Data.Map.Strict as Map

class DiffExpr a where
  {- | Takes a dictionary of variable identifiers and 
       values, and uses it to compute the 'Expr' fully.
  -}
  eval :: Map.Map String a -> Expr a -> a
  {- | Takes a possibly incomplete dictionary and uses 
       it to reduce 'Expr' as much as possible.
  -}
  simplify :: Map.Map String a -> Expr a -> Expr a
  -- |  Given an identifier, differentiates IN TERMS of that identifier.
  partDiff :: String -> Expr a -> Expr a
  -- | Addition wrapper with simplification
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  -- | Subtraction wrapper with simplifition
  (!-) :: Expr a -> Expr a -> Expr a
  e1 !- e2 = simplify (Map.fromList []) $ Subt e1 e2
  -- | Multiplication wrapper with simplifition
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  -- | Division wrapper with simplifition
  (!/) :: Expr a -> Expr a -> Expr a
  e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2
  -- | Value wrapper
  val :: a -> Expr a
  val x = Const x
  -- | Variable wrapper
  var :: String -> Expr a
  var x = Var x
  -- | Sine wrapper with simplification
  mySin :: Expr a -> Expr a
  mySin e = simplify (Map.fromList []) $ Sin e
  -- | Cosine wrapper with simplification
  myCos :: Expr a -> Expr a
  myCos e = simplify (Map.fromList []) $ Cos e
  -- | Natural exponentiation wrapper with simplification
  myExp :: Expr a -> Expr a
  myExp e = simplify (Map.fromList []) $ Exp e
  -- | Natural logarithm wrapper with simplification
  myLn :: Expr a -> Expr a
  myLn e = simplify (Map.fromList []) $ Ln e
  
instance (Floating a, Eq a) => DiffExpr a where
  eval vrs (Add e1 e2)  = (eval vrs e1) + (eval vrs e2)
  eval vrs (Subt e1 e2)  = (eval vrs e1) - (eval vrs e2)
  eval vrs (Mult e1 e2) = (eval vrs e1) * (eval vrs e2)
  eval vrs (Div e1 e2) = (eval vrs e1) / (eval vrs e2)
  eval vrs (Sin e) = sin (eval vrs e)
  eval vrs (Cos e) = cos (eval vrs e)
  eval vrs (Exp e) = exp (eval vrs e)
  eval vrs (Ln e) = log (eval vrs e)
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"

  --Simplify Addition
  simplify vrs (Add e1 (Const 0)) = simplify vrs e1
  simplify vrs (Add (Const 0) e2) = simplify vrs e2
  simplify vrs (Add (Const a) (Const b)) = Const (eval vrs (Add (Const a) (Const b)))
  simplify vrs (Add e1 e2) = (Add (simplify vrs e1) (simplify vrs e2))
  
  --Simplify Subtraction
  simplify vrs (Subt e1 (Const 0)) = simplify vrs e1
  simplify vrs (Subt (Const 0) e2) = simplify vrs e2
  simplify vrs (Subt (Const a) (Const b)) = Const (eval vrs (Subt (Const a) (Const b)))
  simplify vrs (Subt e1 e2) = (Subt (simplify vrs e1) (simplify vrs e2))
  
  --Simplify Multiplication
  simplify vrs (Mult e1 (Const 0)) = Const 0
  simplify vrs (Mult (Const 0) e2) = Const 0
  simplify vrs (Mult e1 (Const 1)) = simplify vrs e1
  simplify vrs (Mult (Const 1) e2) = simplify vrs e2
  simplify vrs (Mult (Const a) (Const b)) = Const (eval vrs (Mult (Const a) (Const b)))
  simplify vrs (Mult e1 e2) = (Mult (simplify vrs e1) (simplify vrs e2))
  
  --Simplify Divison
  simplify vrs (Div (Const 0) _) = Const 0
  simplify vrs (Div e1 (Const 1)) = simplify vrs e1
  simplify vrs (Div (Const a) (Const b)) = Const (eval vrs (Div (Const a) (Const b)))
  simplify vrs (Div e1 e2) = (Div (simplify vrs e1) (simplify vrs e2))
  
  --Simplify Sine
  simplify vrs (Sin (Const a)) = Const (eval vrs (Sin (Const a)))
  simplify vrs (Sin e) = Sin (simplify vrs e)
  
  --Simplify Cosine
  simplify vrs (Cos (Const a)) = Const (eval vrs (Cos (Const a)))
  simplify vrs (Cos e) = Cos (simplify vrs e)
  
  --Simplify Natural Exponentiation
  simplify vrs (Exp (Const a)) = Const (eval vrs (Exp (Const a)))
  simplify vrs (Exp e) = Exp (simplify vrs e)
  
  --Simplify Natural Logarithm
  simplify vrs (Ln (Const a)) = Const (eval vrs (Ln (Const a)))
  simplify vrs (Ln e) = Ln (simplify vrs e)
  
  --Simplify Constants
  simplify vrs (Const x) = Const x
  
  --Simplify Variables
  simplify vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> Const v
                       Nothing -> Var x
 
 --Partial Differentiation
  partDiff _ (Const _) = Const 0
  partDiff v (Var x) = if x == v
                       then
                          Const 1
                       else
                          Const 0
  partDiff v (Div e1 e2) = Div (Subt (Mult (partDiff v e1) e2) (Mult e1 (partDiff v e2))) (Mult e2 e2) --Differentiates using Quotient Rule
  partDiff v (Add e1 e2) = Add (partDiff v e1) (partDiff v e2)
  partDiff v (Mult e1 e2) = Add (Mult (partDiff v e1) e2) (Mult e1 (partDiff v e2)) --Differentiates using Product Rule
  partDiff v (Sin e) = Mult (partDiff v e) (Cos e)
  partDiff v (Cos e) = Mult (partDiff v e) (Mult (Const (-1)) (Sin e))
  partDiff v (Exp e) = Mult (partDiff v e) (Exp e)
  partDiff v (Ln e) = Div (partDiff v e) e