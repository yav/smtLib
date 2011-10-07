{-# LANGUAGE OverloadedStrings #-}
module SMTLib1.QF_AUFBV (module SMTLib1.QF_AUFBV, module X) where

import SMTLib1.QF_BV as X

-- | 'tArray i n' is an array indexed by bitvectors of widht 'i',
-- and storing bitvectors of width 'n'.
tArray :: Integer -> Integer -> Sort
tArray x y = I "Array" [x,y]

-- | @select array index@
select :: Term -> Term -> Term
select a i = App "select" [a,i]

-- | @store array index value@
store :: Term -> Term -> Term -> Term
store a i v = App "store" [a,i,v]

