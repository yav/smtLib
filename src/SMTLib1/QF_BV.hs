{-# LANGUAGE OverloadedStrings #-}
module SMTLib1.QF_BV (module X, module SMTLib1.QF_BV) where

import SMTLib1 as X
import GHC.Exts(IsString(..))


tBitVec :: Integer -> Sort
tBitVec n = I "BitVec" [n]

isBitVec :: Sort -> Maybe Integer
isBitVec (I "BitVec" [n]) = Just n
isBitVec _                = Nothing

-- | BitVec[1]
bit0 :: Term
bit0 = App "bit0" []

-- | BitVec[1]
bit1 :: Term
bit1 = App "bit1" []

-- | [m] -> [n] -> [m+n]
concat :: Term -> Term -> Term
concat x y = App "concat" [x,y]

extract :: Integer -> Integer -> Term -> Term
extract i j t = App (I "extract" [i,j]) [t]

bvnot :: Term -> Term
bvnot t = App "bvnot" [t]

bvand :: Term -> Term -> Term
bvand s t = App "bvand" [s,t]

bvor :: Term -> Term -> Term
bvor s t = App "bvor" [s,t]

bvneg :: Term -> Term
bvneg t  = App "bvneg" [t]

bvadd :: Term -> Term -> Term
bvadd s t = App "bvadd" [s,t]

bvmul :: Term -> Term -> Term
bvmul s t = App "bvmul" [s,t]

bvudiv :: Term -> Term -> Term
bvudiv s t = App "bvudiv" [s,t]

bvurem :: Term -> Term -> Term
bvurem s t = App "bvurem" [s,t]

bvshl :: Term -> Term -> Term
bvshl s t = App "bvshl" [s,t]

bvlshr :: Term -> Term -> Term
bvlshr s t = App "bvlshr" [s,t]


bv :: Integer -> Integer -> Term
bv x m = if x >= 0 then lit x else bvneg (lit (negate x))
  where lit y = App (I (fromString ("bv" ++ show y)) [m]) []

bvnand :: Term -> Term -> Term
bvnand s t = App "bvnand" [s,t]

bvnor :: Term -> Term -> Term
bvnor s t = App "bvnor" [s,t]

bvxor :: Term -> Term -> Term
bvxor s t = App "bvxor" [s,t]

bvxnor :: Term -> Term -> Term
bvxnor s t = App "bvxnor" [s,t]

bvcomp :: Term -> Term -> Term
bvcomp s t = App "bvcomp" [s,t]

bvsub :: Term -> Term -> Term
bvsub s t = App "bvsub" [s,t]

bvsdiv :: Term -> Term -> Term
bvsdiv s t = App "bvsdiv" [s,t]

bvsrem :: Term -> Term -> Term
bvsrem s t = App "bvsrem" [s,t]

bvsmod :: Term -> Term -> Term
bvsmod s t = App "bvsmod" [s,t]

bvashr :: Term -> Term -> Term
bvashr s t = App "bvashr" [s,t]

repeat :: Integer -> Term -> Term
repeat i t = App (I "repeat" [i]) [t]

zero_extend :: Integer -> Term -> Term
zero_extend i t = App (I "zero_extend" [i]) [t]

sign_extend :: Integer -> Term -> Term
sign_extend i t = App (I "sign_extend" [i]) [t]

rotate_left :: Integer -> Term -> Term
rotate_left i t = App (I "rotate_left" [i]) [t]

rotate_right :: Integer -> Term -> Term
rotate_right i t = App (I "rotate_right" [i]) [t]

bvule :: Term -> Term -> Formula
bvule s t = FPred "bvule" [s,t]

bvugt :: Term -> Term -> Formula
bvugt s t = FPred "bvugt" [s,t]

bvuge :: Term -> Term -> Formula
bvuge s t = FPred "bvuge" [s,t]

bvslt :: Term -> Term -> Formula
bvslt s t = FPred "bvslt" [s,t]

bvsle :: Term -> Term -> Formula
bvsle s t = FPred "bvsle" [s,t]

bvsgt :: Term -> Term -> Formula
bvsgt s t = FPred "bvsgt" [s,t]

bvsge :: Term -> Term -> Formula
bvsge s t = FPred "bvsge" [s,t]



