{-|
Module : ExprParser
Description : Parses strings into expression of Expr
Copyright : (c) Seda Mete @2018
License : WTFPL
Maintainer : metes1@mcmaster.ca
Stability : experimental
Portability : POSIX

This module contains parsers that parse a string into an expression of Expr.
Module is incomplete, functions: Sine, Cosine, Ln, and Exp can't be parsed.
-}

module ExprParser (parseExprD,parseExprF) where

import ExprType

import Text.Parsec
import Text.Parsec.String

{- parseExprD and parseExprF Format
 - Input: "4"     Output: (val 4.0)
 - Input: "x"     Output: (var "x")
 - Input: "4+2"   Output: (((val 4.0)) !+ ((val 2.0))
 - Input: "4*2"   Output: (((val 4.0)) !* ((val 2.0))
 - Input: "4/2"   Output: (((val 4.0)) !/ ((val 2.0))
 - Input: "4-2"   Output: (((val 4.0)) !- ((val 2.0))
-}

-- | Parses a string into an Expr Double type.
parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

-- | Parses Double Expression
exprD :: Parser (Expr Double)
exprD = (termD `chainl1` parseOp)

termD :: Parser (Expr Double)
termD = try parseConstD <|> parseVar 

-- | Parses doubles into constants
parseConstD :: Parser (Expr Double)
parseConstD = do { x <- double;
                  return (Const x) }

-- | Parses a string into an Expr Float type
parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

-- | Parses Float Expression
exprF :: Parser (Expr Float)
exprF = (termF `chainl1` parseOp)

termF :: Parser (Expr Float)
termF = try parseConstF <|> parseVar

-- | Parses letter(s) into a variable. 
parseVar :: Parser (Expr a)
parseVar = do { x <- many1 letter;
                return (Var x) }

-- | Parses floats into constants
parseConstF :: Parser (Expr Float)
parseConstF = do { x <- float;
                  return (Const x) }

-- | Parses operators
parseOp :: Parser (Expr a -> Expr a -> Expr a)
parseOp = do { symbol "+"; return Add }
      <|> do { symbol "-"; return Subt }
      <|> do { symbol "*"; return Mult }
      <|> do { symbol "/"; return Div }

-- | Parses Functions
parseFunc :: Parser (Expr a -> Expr a)
parseFunc = do { symbol "sin"; return Sin }
        <|> do { symbol "cos"; return Cos }
        <|> do { symbol "ln"; return Ln }
        <|> do { symbol "exp"; return Exp }


{- Utility Combinators/Parsers -}
parens :: Parser a -> Parser a
parens p = do { char '(';
                cs <- p;
                char ')';
                return cs }

symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

doubleDigits :: Parser String
doubleDigits = do { ds <- try negDigits <|> digits ;
                    rs <- try decimalDigits <|> return "" ;
                    return $ ds ++ rs }

decimalDigits :: Parser String
decimalDigits = do { d <- char '.' ;
                     rm <- digits ;
                     return $ d:rm }

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

double :: Parser Double
double = fmap read $ doubleDigits

float :: Parser Float
float = fmap read $ doubleDigits