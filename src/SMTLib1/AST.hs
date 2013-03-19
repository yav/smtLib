-- This file is based on:
--
-- "The SMT-LIB Standard, Version 1.2"
-- by Silvio Ranise and Cesare Tinelli
-- Release: 5 August 2006
-- Appendix A
--
-- URL:
-- http://goedel.cs.uiowa.edu/smtlib/papers/format-v1.2-r06.08.05.pdf

{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module SMTLib1.AST where

import Data.Typeable
import Data.Data
import GHC.Exts(IsString(..))

newtype Name  = N String
                deriving (Eq,Ord,Show,Data,Typeable)

data Ident    = I Name [Integer]
                deriving (Eq,Ord,Show,Data,Typeable)

data Quant    = Exists | Forall
                deriving (Eq,Ord,Show,Data,Typeable)

data Conn     = Not | Implies | And | Or | Xor | Iff | IfThenElse
                deriving (Eq,Ord,Show,Data,Typeable)

data Formula  = FTrue
              | FFalse
              | FPred Ident [Term]
              | FVar Name
              | Conn Conn [Formula]
              | Quant Quant [Binder] Formula
              | Let Name Term Formula
              | FLet Name Formula Formula
              | FAnnot Formula [Annot]
                deriving (Eq,Ord,Show,Data,Typeable)

type Sort     = Ident

data Binder   = Bind { bindVar :: Name, bindSort :: Sort }
                deriving (Eq,Ord,Show,Data,Typeable)

data Term     = Var Name
              | App Ident [Term]
              | Lit Literal
              | ITE Formula Term Term
              | TAnnot Term [Annot]
                deriving (Eq,Ord,Show,Data,Typeable)

data Literal  = LitNum Integer
              | LitFrac Rational
              | LitStr String
                deriving (Eq,Ord,Show,Data,Typeable)

data Annot    = Attr { attrName :: Name, attrVal :: Maybe String }
                deriving (Eq,Ord,Show,Data,Typeable)

data FunDecl  = FunDecl { funName   :: Ident
                        , funArgs   :: [Sort]
                        , funRes    :: Sort
                        , funAnnots :: [Annot]
                        }
  deriving (Data,Typeable)

data PredDecl = PredDecl { predName   :: Ident
                         , predArgs   :: [Sort]
                         , predAnnots :: [Annot]
                         }
  deriving (Data,Typeable)

data Status   = Sat | Unsat | Unknown
  deriving (Data,Typeable)

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
  deriving (Data,Typeable)

-- aka "benchmark"
data Script = Script { scrName :: Ident, scrCommands :: [Command] }


--------------------------------------------------------------------------------
-- To make it a bit simpler to write terms in the above AST
-- we provide some instances.  They are intended to be used only
-- for writing simple literals, and not any of the computational
-- operations associated with the classes.

-- Strings
instance IsString Name      where fromString x = N x
instance IsString Ident     where fromString x = I (fromString x) []
instance IsString Term      where fromString x = Lit (LitStr x)

-- Integral operations
instance Num Term where
  fromInteger = Lit . LitNum
  x + y       = App "+" [x,y]
  x - y       = App "-" [x,y]
  x * y       = App "*" [x,y]
  signum x    = App "signum" [x]
  abs x       = App "abs" [x]

-- Fractional numbers
instance Fractional Term where
  fromRational  = Lit . LitFrac . fromRational
  x / y         = App "/" [x,y]

--------------------------------------------------------------------------------
-- XXX: move to a separate module?

(===) :: Term -> Term -> Formula
x === y   = FPred "=" [x,y]

(=/=) :: Term -> Term -> Formula
x =/= y   = FPred "distinct" [x,y]

-- | For 'Int'
(.<.) :: Term -> Term -> Formula
x .<. y   = FPred "<" [x,y]

-- | For 'Int'
(.>.) :: Term -> Term -> Formula
x .>. y   = FPred ">" [x,y]

tInt :: Sort
tInt = "Int"

funDef :: Ident -> [Sort] -> Sort -> Command
funDef x as b = CmdExtraFuns [ FunDecl { funName = x
                                       , funArgs = as
                                       , funRes = b
                                       , funAnnots = []
                                       } ]

constDef :: Ident -> Sort -> Command
constDef x t = funDef x [] t


logic :: Ident -> Command
logic = CmdLogic

assume :: Formula -> Command
assume = CmdAssumption

goal :: Formula -> Command
goal = CmdFormula




