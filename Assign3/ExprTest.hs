{-|
Module : ExprTest
Description : Contains a type class and instances for differentiable expressions
Copyright : (c) Seda Mete @2018
License : WTFPL
Maintainer : metes1@mcmaster.ca
Stability : experimental
Portability : POSIX
TODO write a longer description of the module,
containing some commentary with @some markup@.
-}

module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

sampleExpr1 :: Expr Float
sampleExpr1 = Add (Mult (Var "x") (Add (Const 3) (Mult (Const 0) (Const 3)))) (Var "y")


listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"

test1 :: Float -> Float
test1 x = eval (Map.fromList [("x",x)]) sampleExpr1