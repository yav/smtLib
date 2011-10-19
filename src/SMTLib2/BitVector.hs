{-# LANGUAGE OverloadedStrings #-}
module SMTLib2.BitVector where

import SMTLib2.AST

tBitVec :: Integer -> Type
tBitVec n = TApp (I "BitVec" [n]) []

concat :: Expr -> Expr -> Expr
concat x y = app "concat" [x,y]

extract :: Integer -> Integer -> Expr -> Expr
extract i j x = app (I "extract" [i,j]) [x]

bvnot :: Expr -> Expr
bvnot x = app "bvnot" [x]

bvneg :: Expr -> Expr
bvneg x = app "bvneg" [x]

bvand :: Expr -> Expr -> Expr
bvand x y = app "bvand" [x,y]

bvor :: Expr -> Expr -> Expr
bvor x y = app "bvor" [x,y]

bvmul :: Expr -> Expr -> Expr
bvmul x y = app "bvmul" [x,y]

bvudiv :: Expr -> Expr -> Expr
bvudiv x y = app "bvudiv" [x,y]

bvurem :: Expr -> Expr -> Expr
bvurem x y = app "bvurem" [x,y]

bvshl :: Expr -> Expr -> Expr
bvshl x y = app "bvshl" [x,y]

bvlshr :: Expr -> Expr -> Expr
bvlshr x y = app "bvlshr" [x,y]

bvult :: Expr -> Expr -> Expr
bvult x y = app "bvult" [x,y]

bv :: Integer -> Integer -> Expr
bv num w = Lit (LitBV num w)

