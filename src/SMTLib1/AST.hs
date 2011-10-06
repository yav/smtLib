-- This file is based on:
--
-- "The SMT-LIB Standard, Version 1.2"
-- by Silvio Ranise and Cesare Tinelli
-- Release: 5 August 2006
-- Appendix A
--
-- URL:
-- http://goedel.cs.uiowa.edu/smtlib/papers/format-v1.2-r06.08.05.pdf

{-# LANGUAGE OverloadedStrings #-}
module SMTLib1.AST where

import GHC.Exts(IsString(..))

newtype Name  = N String
                deriving (Eq,Ord,Show)

data Ident    = I Name [Integer]
                deriving (Eq,Ord,Show)

data Quant    = Exists | Forall
                deriving (Eq,Ord,Show)

data Conn     = Not | Implies | And | Or | Xor | Iff | IfThenElse
                deriving (Eq,Ord,Show)

data Formula  = FTrue
              | FFalse
              | FPred Ident [Term]
              | FVar Name
              | Conn Conn [Formula]
              | Quant Quant [Binder] Formula
              | Let Name Term Formula
              | FLet Name Formula Formula
              | FAnnot Formula [Annot]
                deriving (Eq,Ord,Show)

type Sort     = Ident

data Binder   = Bind { bindVar :: Name, bindSort :: Sort }
                deriving (Eq,Ord,Show)

data Term     = Var Name
              | App Ident [Term]
              | Lit Literal
              | ITE Formula Term Term
              | TAnnot Term [Annot]
                deriving (Eq,Ord,Show)

data Literal  = LitNum Integer
              | LitFrac Double     -- Is this good enough?
              | LitStr String
                deriving (Eq,Ord,Show)

data Annot    = Attr { attrName :: Name, attrVal :: Maybe String }
                deriving (Eq,Ord,Show)

data FunDecl  = FunDecl { funName   :: Ident
                        , funArgs   :: [Sort]
                        , funRes    :: Sort
                        , funAnnots :: [Annot]
                        }

data PredDecl = PredDecl { predName   :: Ident
                         , predArgs   :: [Sort]
                         , predAnnots :: [Annot]
                         }

data Status   = Sat | Unsat | Unknown

data Command
  = CmdLogic Ident
  | CmdAssumption Formula
  | CmdFormula Formula
  | CmdStatus Status
  | CmdExtraSorts [ Sort ]
  | CmdExtraFuns  [ FunDecl ]
  | CmdExtraPreds [ PredDecl ]
  | CmdNotes String
  | CmdAnnot Annot

-- aka "benchmark"
data Script = Script { scrName :: Ident, scrCommands :: [Command] }


--------------------------------------------------------------------------------
-- To make it a bit simpler to write terms in the above AST
-- we provide some instances.  They are intended to be used only
-- for writing simple literals, and not any of the computational
-- operations associated with the classes.

-- Strings
instance IsString Name      where fromString = N
instance IsString Ident     where fromString x = I (fromString x) []
instance IsString Term      where fromString   = Lit . fromString
instance IsString Literal   where fromString = LitStr

-- Integers
instance Num Literal where
  fromInteger = LitNum
  (+)     = error "Literal: (+)"
  (*)     = error "Literal: (*)"
  signum  = error "Literal: signum"
  abs     = error "Literal: abs"

instance Num Term where
  fromInteger = Lit . fromInteger
  (+)     = error "Term: (+)"
  (*)     = error "Term: (*)"
  signum  = error "Term: signum"
  abs     = error "Term: abs"

-- Fractional numbers
instance Fractional Literal where fromRational = LitFrac . fromRational
instance Fractional Term    where fromRational = Lit . fromRational



