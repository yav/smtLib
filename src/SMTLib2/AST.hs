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

data Ident    = I Name [Integer]
                deriving (Eq,Ord,Show)

data Quant    = Exists | Forall
                deriving (Eq,Ord,Show)

data Binder   = Bind { bindVar :: Name, bindType :: Type }
                deriving (Eq,Ord,Show)

data Defn     = Defn { defVar :: Name, defExpr :: Expr }
                deriving (Eq,Ord,Show)

data Literal  = LitBV Integer Integer   -- ^ value, width (in bits)
              | LitNum Integer
              | LitFrac Rational
              | LitStr String
                deriving (Eq,Ord,Show)

data Type     = TApp Ident [Type]
              | TVar Name
                deriving (Eq,Ord,Show)

data Expr     = Lit Literal
              | App Ident (Maybe Type) [Expr]
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
instance IsString Name      where fromString   = N
instance IsString Ident     where fromString x = I (fromString x) []
instance IsString Type      where fromString x = TApp (fromString x) []
instance IsString Expr      where fromString   = Lit . LitStr . fromString

-- Integers

-- NOTE: Some of these might not mean anything, depending on the theory.
instance Num Expr where
  fromInteger x = Lit (LitNum x)
  x + y         = app "+"      [x,y]
  x - y         = app "-"      [x,y]
  x * y         = app "*"      [x,y]
  signum x      = app "signum" [x]
  abs x         = app "abs"    [x]


-- Fractional numbers
instance Fractional Expr where
  fromRational x  = Lit (LitFrac x)
  x / y           = app "/" [x,y]

app :: Ident -> [Expr] -> Expr
app f es = App f Nothing es






