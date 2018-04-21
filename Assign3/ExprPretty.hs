{-|
Module : ExprPretty
Description : Contains a Show instance for Expr datatype
Copyright : (c) Seda Mete @2018
License : WTFPL
Maintainer : metes1@mcmaster.ca
Stability : experimental
Portability : POSIX

This module contains a Show instance for the 'Expr' datatype.
It prints out expressions in a more readable and pretty format.
-}

module ExprPretty where

import ExprType

-- | A function that wraps expressions in parentheses.
parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- | Instance Show Expr: Provides a pretty representation of the 'Expr' datatype, matching the DSL provided in 'DiffExpr'
instance Show a => Show (Expr a) where
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Subt e1 e2)  = parens (show e1) ++ " !- " ++ parens (show e2)
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Div e1 e2) = parens (show e1) ++ " !/ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
  show (Sin e)      = parens $ "sin " ++ show e
  show (Cos e)      = parens $ "cos " ++ show e
  show (Exp e)      = parens $ "exp " ++ show e
  show (Ln e)       = parens $ "ln " ++ show e