{-# LANGUAGE OverloadedStrings #-}
module SMTLib2.Array where

import SMTLib2.AST

tArray :: Type -> Type -> Type
tArray x y = TApp "Array" [x,y]

select :: Expr -> Expr -> Expr
select x y = app "select" [x,y]

store :: Expr -> Expr -> Expr -> Expr
store x y z = app "store" [x,y,z]


