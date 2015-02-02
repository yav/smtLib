{-# LANGUAGE Safe #-}
module SMTLib2.PP where

import SMTLib2.AST
import Text.PrettyPrint
import Numeric
import Data.List(genericReplicate)

class PP t where
  pp :: t -> Doc

instance PP Bool where
  pp True   = text "true"
  pp False  = text "false"

instance PP Integer where
  pp        = integer

ppString :: String -> Doc
ppString = text . show

instance PP Name where
  pp (N x) = text x

instance PP Ident where
  pp (I x []) = pp x
  pp (I x is) = parens (char '_' <+> pp x <+> fsep (map integer is))

instance PP Attr where
  pp (Attr x v) = char ':' <> pp x <+> maybe empty pp v

instance PP Quant where
  pp Forall = text "forall"
  pp Exists = text "exists"

instance PP Expr where
  pp expr =
    case expr of

      Lit l     -> pp l

      App c ty ts  ->
        case ts of
          [] -> ppFun
          _  -> parens (ppFun <+> fsep (map pp ts))

        where ppFun = case ty of
                        Nothing -> pp c
                        Just t  -> parens (text "as" <+> pp c <+> pp t)

      Quant q bs e ->
        case bs of
          [] -> pp e
          _  -> parens (pp q <+> parens (fsep (map pp bs)) $$ nest 2 (pp e))

      Let ds e ->
        case ds of
          [] -> pp e
          _  -> parens (text "let" <+> (parens (vcat (map pp ds)) $$ pp e))

      Annot e as ->
        case as of
          [] -> pp e
          _  -> parens (char '!' <+> pp e $$ nest 2 (vcat (map pp as)))


instance PP Binder where
  pp (Bind x t) = parens (pp x <+> pp t)

instance PP Defn where
  pp (Defn x e)   = parens (pp x <+> pp e)

instance PP Type where
  pp ty =
    case ty of
      TApp c ts ->
        case ts of
          [] -> pp c
          _  -> parens (pp c <+> fsep (map pp ts))
      TVar x -> pp x

instance PP Literal where
  pp lit =
    case lit of

      LitBV n w ->
        case divMod w 4 of
          -- For the moment we do not print using HEX literals as
          -- some solvers do not support them (how hard is that???)
          -- (x,0) -> text "#x" <> text (pad x (showHex v ""))
          _ -> text "#b" <> text (pad w (showIntAtBase 2 (head . show) v ""))

        where pad digs xs = genericReplicate
                                (digs - fromIntegral (length xs)) '0' ++ xs

              v = if n < 0 then 2^w + n else n

      LitNum n -> integer n

      LitFrac x -> text (show (fromRational x :: Double))  -- XXX: Good enough?

      LitStr x -> ppString x



instance PP Option where
  pp opt =
    case opt of
      OptPrintSuccess b             -> std "print-success" b
      OptExpandDefinitions b        -> std "expand-definitions" b
      OptInteractiveMode b          -> std "interactive-mode" b
      OptProduceProofs b            -> std "produce-proofs" b
      OptProduceUnsatCores b        -> std "produce-unsat-cores" b
      OptProduceModels b            -> std "produce-models" b
      OptProduceAssignments b       -> std "produce-assignments" b
      OptRegularOutputChannel s     -> str "regular-output-channel" s
      OptDiagnosticOutputChannel s  -> str "diagnostic-output-channel" s
      OptRandomSeed n               -> std "random-seed" n
      OptVerbosity n                -> std "verbosity" n
      OptAttr a                     -> pp a

    where mk a b  = char ':' <> text a <+> b
          std a b = mk a (pp b)
          str a b = mk a (ppString b)

instance PP InfoFlag where
  pp info =
    case info of
      InfoAllStatistics -> mk "all-statistics"
      InfoErrorBehavior -> mk "error-behavior"
      InfoName          -> mk "name"
      InfoAuthors       -> mk "authors"
      InfoVersion       -> mk "version"
      InfoStatus        -> mk "status"
      InfoReasonUnknown -> mk "reason-unknown"
      InfoAttr a        -> pp a
    where mk x = char ':' <> text x

instance PP Command where
  pp cmd =
    case cmd of
      CmdSetLogic n     -> std "set-logic" n
      CmdSetOption o    -> std "set-option" o
      CmdSetInfo a      -> std "set-info" a
      CmdDeclareType x n    -> mk "declare-sort" (pp x <+> integer n)
      CmdDefineType x as t  -> fun "define-sort" x as (pp t)
      CmdDeclareFun x ts t  -> fun "declare-fun" x ts (pp t)
      CmdDefineFun x bs t e -> fun "define-fun" x bs (pp t $$ nest 2 (pp e))
      CmdPush n         -> std "push" n
      CmdPop n          -> std "pop" n
      CmdAssert e       -> std "assert" e
      CmdCheckSat       -> one "check-sat"
      CmdGetAssertions  -> one "get-assertions"
      CmdGetValue es    -> mk  "get-value" (parens (fsep (map pp es)))
      CmdGetProof       -> one "get-proof"
      CmdGetUnsatCore   -> one "get-unsat-core"
      CmdGetInfo i      -> std "get-info" i
      CmdGetOption n    -> std "get-option" n
      CmdComment s      -> vcat (map comment (lines s))
      CmdExit           -> one "exit"
    where mk x d = parens (text x <+> d)
          one x   = mk x empty
          std x a = mk x (pp a)
          fun x y as d = mk x (pp y <+> parens (fsep (map pp as)) <+> d)
          comment s = text ";" <+> text s

instance PP Script where
  pp (Script cs) = vcat (map pp cs)

