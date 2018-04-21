{-|
Module : ExprType
Description : Contains a datatype for numeric expression
Copyright : (c) Seda Mete @2018
License : WTFPL
Maintainer : metes1@mcmaster.ca
Stability : experimental
Portability : POSIX

This module contains the datatype 'Expr', which wraps different operations
in an expression tree. It encodes addition, subtraction, multiplication, 
division, sine, cosine, natural exponentiation, the natural logarithm, 
constants, and variables.
-}

module ExprType where

import           Data.List

-- * DataType Declaration
-- | A datatype for common numeric expression
data Expr a = Add (Expr a) (Expr a) -- ^ Binary Addition
            | Subt (Expr a) (Expr a) -- ^ Binary Subtraction
            | Mult (Expr a) (Expr a) -- ^ Binary Multiplication
            | Div (Expr a) (Expr a) -- ^ Division
            | Sin (Expr a) -- ^ Sine Function
            | Cos (Expr a) -- ^ Cosine Function
            | Exp (Expr a) -- ^ Natural Exponentiation: Exp a = e^a
            | Ln (Expr a) -- ^ Natural Logarithm
            | Const a -- ^ Value Wrapper
            | Var String -- ^ Variable Identifier
  deriving (Eq)

-- * Miscellaneous Functions
-- | A function that retrieves variable identifiers from an 'Expr'.
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Subt e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Div e1 e2) = getVars e1 `union` getVars e2
getVars (Sin e)      = getVars e
getVars (Cos e)      = getVars e
getVars (Exp e)      = getVars e
getVars (Ln e)       = getVars e
getVars (Const _)    = []
getVars (Var ident)  = [ident]