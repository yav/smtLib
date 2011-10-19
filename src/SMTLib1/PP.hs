module SMTLib1.PP where

import SMTLib1.AST
import Text.PrettyPrint

class PP t where
  pp :: t -> Doc

instance PP Name where
  pp (N x) = text x

instance PP Ident where
  pp (I x is) = pp x <> case is of
                          [] -> empty
                          _  -> brackets $ hcat $ punctuate (char ':')
                                                $ map integer is

instance PP Quant where
  pp Forall = text "forall"
  pp Exists = text "exists"

instance PP Conn where
  pp conn =
    case conn of
      Not        -> text "not"
      Implies    -> text "implies"
      And        -> text "and"
      Or         -> text "or"
      Xor        -> text "xor"
      Iff        -> text "iff"
      IfThenElse -> text "if_then_else"

instance PP Binder where
  pp (Bind x t) = parens (char '?' <> pp x <+> pp t)

instance PP Formula where
  pp form =
    case form of
      FTrue       -> text "true"
      FFalse      -> text "false"
      FVar x      -> char '$' <> pp x
      FPred p []  -> pp p
      _           -> parens (ppUnwrap form)

    where
    ppUnwrap form1 =
      case form1 of
        Conn c fs     -> pp c <+> fsep (map pp fs)
        Quant q bs f  ->
          case bs of
            [] -> pp f
            _  -> pp q <+> sep (map pp bs) <+> pp f
        Let x t f     -> text "let" <+> parens (char '?' <> pp x <+> pp t)
                      $$ pp f
        FLet x f1 f2  -> text "flet" <+> parens (char '$' <> pp x <+> pp f1)
                      $$ pp f2
        FPred p ts    -> pp p <+> fsep (map pp ts)
        FAnnot f as   -> ppUnwrap f $$ nest 2 (vcat (map pp as))
        _             -> pp form1


instance PP Annot where
  pp (Attr x v) = char ':' <> pp x <+> maybe empty ppUserValue v
    where
    ppUserValue = braces . text . concatMap esc

    esc '{' = "\\{"
    esc c   = [c]

instance PP Term where
  pp term =
    case term of
      Var n     -> char '?' <> pp n
      App f []  -> pp f
      Lit l     -> pp l
      _         -> parens (ppUnwrap term)

    where
    ppUnwrap term1 =
      case term1 of
        App f ts    -> pp f <+> fsep (map pp ts)
        ITE f t1 t2 -> text "ite" <+> pp f $$ nest 2 (pp t1 $$ pp t2)
        TAnnot t as -> ppUnwrap t $$ nest 2 (vcat (map pp as))
        _           -> pp term1


instance PP Literal where
  pp lit =
    case lit of
      LitNum n  -> integer n
      LitFrac x -> text (show x)
      LitStr x  -> text (show x)

instance PP FunDecl where
  pp d = parens (pp (funName d) <+> fsep (map pp (funArgs d)) <+> pp (funRes d)
                  $$ nest 2 (vcat (map pp (funAnnots d))))

instance PP PredDecl where
  pp d = parens (pp (predName d) <+> fsep (map pp (predArgs d))
                  $$ nest 2 (vcat (map pp (predAnnots d))))

instance PP Status where
  pp stat =
    case stat of
      Sat       -> text "sat"
      Unsat     -> text "unsat"
      Unknown   -> text "unknown"

instance PP Command where
  pp cmd =
    case cmd of
      CmdLogic n      -> std "logic" n
      CmdAssumption f -> std "assumption" f
      CmdFormula f    -> std "formula" f
      CmdStatus s     -> std "status" s
      CmdExtraSorts s -> many "extrasorts" s
      CmdExtraFuns  f -> many "extrafuns" f
      CmdExtraPreds p -> many "extrapreds" p
      CmdNotes s      -> mk "notes" (str s)
      CmdAnnot a      -> pp a

    where mk x d    = char ':' <> text x <+> d

          std x n   = mk x (pp n)

          many _ [] = empty
          many x ns = mk x (parens (vcat (map pp ns)))

          esc '"'   = "\\\""
          esc c     = [c]

          str s     = (char '"' <> text (concatMap esc s) <> char '"')





instance PP Script where
  pp s  = parens (text "benchmark" <+> pp (scrName s)
                  $$ nest 2 (vcat (map pp (scrCommands s))))

