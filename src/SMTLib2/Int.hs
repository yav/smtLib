{-# LANGUAGE OverloadedStrings, Safe #-}
module SMTLib2.Int where

import SMTLib2.AST

tInt :: Type
tInt = TApp (I "Int" []) []

num :: Integral a => a -> Expr
num a = Lit (LitNum (toInteger a))

nNeg :: Expr -> Expr
nNeg x = app "-" [x]

nSub :: Expr -> Expr -> Expr
nSub x y = app "-" [x,y]

nAdd :: Expr -> Expr -> Expr
nAdd x y = app "+" [x,y]

nMul :: Expr -> Expr -> Expr
nMul x y = app "*" [x,y]

nDiv :: Expr -> Expr -> Expr
nDiv x y = app "div" [x,y]

nMod :: Expr -> Expr -> Expr
nMod x y = app "mod" [x,y]

nAbs :: Expr -> Expr
nAbs x = app "abs" [x]

nLeq :: Expr -> Expr -> Expr
nLeq x y = app "<=" [x,y]

nLt :: Expr -> Expr -> Expr
nLt x y = app "<" [x,y]

nGeq :: Expr -> Expr -> Expr
nGeq x y = app ">=" [x,y]

nGt :: Expr -> Expr -> Expr
nGt x y = app ">" [x,y]
