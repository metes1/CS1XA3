{-|
Module : ExprParser
Description : Parses string into expression of Expr
Copyright : (c) Seda Mete @2018
License : WTFPL
Maintainer : metes1@mcmaster.ca
Stability : experimental
Portability : POSIX

This module contains parsers that parse a string into an expression of Expr
-}

module ExprParser (parseExprD,parseExprF) where

import ExprType

import Text.Parsec
import Text.Parsec.String

-- | Parses a string into an Expr Double type using the Parsec package
parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

-- | Parses a string into an Expr Float type using the Parsec package
parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

exprD :: Parser (Expr Double)
exprD = error "define me!" -- #TODO complete parser

exprF :: Parser (Expr Float)
exprF = error "define me!" -- #TODO complete parser