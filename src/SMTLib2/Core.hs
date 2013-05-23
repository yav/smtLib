{-# LANGUAGE OverloadedStrings, Safe #-}
module SMTLib2.Core where

import SMTLib2.AST

tBool :: Type
tBool = "Bool"

true :: Expr
true = app "true" []

false :: Expr
false = app "false" []

not :: Expr -> Expr
not p = app "not" [p]

(==>) :: Expr -> Expr -> Expr
p ==> q = app "=>" [p,q]

and :: Expr -> Expr -> Expr
and p q = app "and" [p,q]

or :: Expr -> Expr -> Expr
or p q = app "or" [p,q]

xor :: Expr -> Expr -> Expr
xor p q = app "xor" [p,q]

(===) :: Expr -> Expr -> Expr
x === y = app "=" [x,y]

(=/=) :: Expr -> Expr -> Expr
x =/= y = app "distinct" [x,y]

ite :: Expr -> Expr -> Expr -> Expr
ite b x y = app "ite" [b,x,y]




