{-# LANGUAGE Safe #-}
module SMTLib1
  ( Name(..)
  , Ident(..)
  , Quant(..)
  , Conn(..)
  , Formula(..)
  , Sort
  , Binder(..)
  , Term(..)
  , Literal(..)
  , Annot(..)
  , FunDecl(..)
  , PredDecl(..)
  , Status(..)
  , Command(..)
  , Script(..)

  , (===)
  , (=/=)
  , (.<.)
  , (.>.)
  , tInt
  , funDef
  , constDef
  , logic
  , assume
  , goal
  ) where

import SMTLib1.AST
import SMTLib1.PP()

