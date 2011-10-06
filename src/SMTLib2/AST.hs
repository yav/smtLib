-- This file is based on:
--
-- "The SMT-LIB Standard, Version 2.0"
-- by Clark Barrett Aaron Stump Cesare Tinelli
-- Release: December 21, 2010
-- Appendix C
--
-- URL:
-- http://goedel.cs.uiowa.edu/smtlib/papers/smt-lib-reference-v2.0-r10.12.21.pdf

{-# LANGUAGE OverloadedStrings #-}
module SMTLib2.AST where

import GHC.Exts(IsString(..))

newtype Name  = N String
                deriving (Eq,Ord,Show)

data Quant    = Exists | Forall
                deriving (Eq,Ord,Show)

data Binder   = Bind { bindVar :: Name, bindType :: Type }
                deriving (Eq,Ord,Show)

data Defn     = Defn { defVar :: Name, defExpr :: Expr }
                deriving (Eq,Ord,Show)


data Format   = Dec | Hex | Bin
                deriving (Eq,Ord,Show)

data Literal  = LitNum Integer Format
              | LitFrac Double     -- Is this good enough?
              | LitStr String
                deriving (Eq,Ord,Show)

data QName    = Name Name
              | TypedName Name Type
                deriving (Eq,Ord,Show)

data Type     = TApp Name [Type]
              | TVar Name
                deriving (Eq,Ord,Show)

data Expr     = Lit Literal
              | App QName [Expr]
              | Quant Quant [Binder] Expr
              | Let [Defn] Expr
              | Annot Expr [Attr]
                deriving (Eq,Ord,Show)

data Attr     = Attr { attrName :: Name , attrVal :: Maybe AttrVal }
                deriving (Eq,Ord,Show)

type AttrVal  = Expr    -- A bit of an approximation....


data Option
  = OptPrintSuccess Bool
  | OptExpandDefinitions Bool
  | OptInteractiveMode Bool
  | OptProduceProofs Bool
  | OptProduceUnsatCores Bool
  | OptProduceModels Bool
  | OptProduceAssignments Bool
  | OptRegularOutputChannel String
  | OptDiagnosticOutputChannel String
  | OptRandomSeed Integer
  | OptVerbosity Integer
  | OptAttr Attr

data InfoFlag
  = InfoAllStatistics
  | InfoErrorBehavior
  | InfoName
  | InfoAuthors
  | InfoVersion
  | InfoStatus
  | InfoReasonUnknown
  | InfoAttr Attr

data Command
  = CmdSetLogic Name
  | CmdSetOption Option
  | CmdSetInfo Attr
  | CmdDeclareType Name Integer
  | CmdDefineType Name [Name] Type
  | CmdDeclareFun Name [Type] Type
  | CmdDefineFun Name [Binder] Type Expr
  | CmdPush Integer
  | CmdPop Integer
  | CmdAssert Expr
  | CmdCheckSat
  | CmdGetAssertions
  | CmdGetValue [Expr]
  | CmdGetProof
  | CmdGetUnsatCore
  | CmdGetInfo InfoFlag
  | CmdGetOption Name
  | CmdExit

newtype Script = Script [Command]


--------------------------------------------------------------------------------
-- To make it a bit simpler to write terms in the above AST
-- we provide some instances.  They are intended to be used only
-- for writing simple literals, and not any of the computational
-- operations associated with the classes.

-- Strings
instance IsString Name      where fromString = N
instance IsString Expr      where fromString = Lit . fromString
instance IsString Literal   where fromString = LitStr

-- Integers
instance Num Literal where
  fromInteger x = LitNum x Dec
  (+)     = error "Literal: (+)"
  (*)     = error "Literal: (*)"
  signum  = error "Literal: signum"
  abs     = error "Literal: abs"

instance Num Expr where
  fromInteger = Lit . fromInteger
  (+)     = error "Expr: (+)"
  (*)     = error "Expr: (*)"
  signum  = error "Expr: signum"
  abs     = error "Expr: abs"

-- Fractional numbers
instance Fractional Literal where fromRational = LitFrac . fromRational
instance Fractional Expr    where fromRational = Lit . fromRational



