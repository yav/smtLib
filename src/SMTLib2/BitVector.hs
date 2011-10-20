{-# LANGUAGE OverloadedStrings #-}
module SMTLib2.BitVector where

import SMTLib2.AST

tBitVec :: Integer -> Type
tBitVec n = TApp (I "BitVec" [n]) []

bv :: Integer -> Integer -> Expr
bv num w = Lit (LitBV num w)

concat :: Expr -> Expr -> Expr
concat x y = app "concat" [x,y]

extract :: Integer -> Integer -> Expr -> Expr
extract i j x = app (I "extract" [i,j]) [x]

bvnot :: Expr -> Expr
bvnot x = app "bvnot" [x]

bvand :: Expr -> Expr -> Expr
bvand x y = app "bvand" [x,y]

bvor :: Expr -> Expr -> Expr
bvor x y = app "bvor" [x,y]

bvneg :: Expr -> Expr
bvneg x = app "bvneg" [x]

bvadd :: Expr -> Expr -> Expr
bvadd x y = app "bvadd" [x,y]

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

bvnand :: Expr -> Expr -> Expr
bvnand x y = app "bvnand" [x,y]

bvnor :: Expr -> Expr -> Expr
bvnor x y = app "bvnor" [x,y]

bvxor :: Expr -> Expr -> Expr
bvxor x y = app "bvxor" [x,y]

bvxnor :: Expr -> Expr -> Expr
bvxnor x y = app "bvxnor" [x,y]

bvcomp :: Expr -> Expr -> Expr
bvcomp x y = app "bvcomp" [x,y]

bvsub :: Expr -> Expr -> Expr
bvsub x y = app "bvsub" [x,y]

bvsdiv :: Expr -> Expr -> Expr
bvsdiv x y = app "bvsdiv" [x,y]

bvsrem :: Expr -> Expr -> Expr
bvsrem x y = app "bvsrem" [x,y]

bvsmod :: Expr -> Expr -> Expr
bvsmod x y = app "bvsmod" [x,y]

bvashr :: Expr -> Expr -> Expr
bvashr x y = app "bvashr" [x,y]

repeat :: Integer -> Expr -> Expr -> Expr
repeat i x y = app (I "repeat" [i]) [x,y]

zero_extend :: Integer -> Expr -> Expr -> Expr
zero_extend i x y = app (I "zero_extend" [i]) [x,y]

sign_extend :: Integer -> Expr -> Expr -> Expr
sign_extend i x y = app (I "sign_extend" [i]) [x,y]

rotate_left :: Integer -> Expr -> Expr -> Expr
rotate_left i x y = app (I "rotate_left" [i]) [x,y]

rotate_right :: Integer -> Expr -> Expr -> Expr
rotate_right i x y = app (I "rotate_right" [i]) [x,y]

bvule :: Expr -> Expr -> Expr
bvule x y = app "bvule" [x,y]

bvugt :: Expr -> Expr -> Expr
bvugt x y = app "bvugt" [x,y]

bvuge :: Expr -> Expr -> Expr
bvuge x y = app "bvuge" [x,y]

bvslt :: Expr -> Expr -> Expr
bvslt x y = app "bvslt" [x,y]

bvsle :: Expr -> Expr -> Expr
bvsle x y = app "bvsle" [x,y]

bvsgt :: Expr -> Expr -> Expr
bvsgt x y = app "bvsgt" [x,y]

bvsge :: Expr -> Expr -> Expr
bvsge x y = app "bvsge" [x,y]









