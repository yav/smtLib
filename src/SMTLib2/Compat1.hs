module SMTLib2.Compat1 where

import qualified SMTLib1.AST as V1
import qualified SMTLib1.PP  as V1
import qualified SMTLib2.AST as V2
import qualified SMTLib2.Core as V2

import Control.Applicative(Applicative(..), (<$>))
import Data.Traversable(traverse)
import Text.PrettyPrint


data Trans a = OK a | Fail Doc

toMaybe :: Trans a -> Maybe a
toMaybe res =
  case res of
    OK a   -> Just a
    Fail _ -> Nothing

toEither :: Trans a -> Either Doc a
toEither res =
  case res of
    OK a     -> Right a
    Fail msg -> Left msg

instance Functor Trans where
  fmap f res =
    case res of
      OK a     -> OK (f a)
      Fail msg -> Fail msg

instance Applicative Trans where
  pure x = OK x

  OK f   <*> OK x   = OK (f x)
  Fail x <*> OK _   = Fail x
  OK _   <*> Fail x = Fail x
  Fail x <*> Fail y = Fail (x $$ y)

err :: Doc -> Trans a
err msg = Fail msg
--------------------------------------------------------------------------------


name :: V1.Name -> V2.Name
name (V1.N x) = V2.N x

ident :: V1.Ident -> V2.Ident
ident (V1.I x ns) = V2.I (name x) ns

quant :: V1.Quant -> V2.Quant
quant q =
  case q of
    V1.Exists -> V2.Exists
    V1.Forall -> V2.Forall

binder :: V1.Binder -> V2.Binder
binder b = V2.Bind { V2.bindVar  = name (V1.bindVar b)
                   , V2.bindType = sort (V1.bindSort b)
                   }

sort :: V1.Sort -> V2.Type
sort x = V2.TApp (ident x) []

literal :: V1.Literal -> V2.Literal
literal lit =
  case lit of
    V1.LitNum n   -> V2.LitNum n
    V1.LitFrac r  -> V2.LitFrac r
    V1.LitStr s   -> V2.LitStr s

term :: V1.Term -> Trans V2.Expr
term te =
  case te of
    V1.Var x       -> pure (V2.app (V2.I (name x) []) []) -- XXX: or add var?
    V1.App i ts    -> V2.app (ident i) <$> traverse term ts
    V1.Lit l       -> pure (V2.Lit (literal l))
    V1.ITE f t1 t2 -> V2.ite <$> formula f <*> term t1 <*> term t2
    V1.TAnnot t a  -> V2.Annot <$> term t <*> traverse annot a


formula :: V1.Formula -> Trans V2.Expr
formula form =
  case form of

    V1.FTrue      -> pure V2.true

    V1.FFalse     -> pure V2.false

    V1.FPred p ts -> V2.app (ident p) <$> traverse term ts

    V1.FVar x     -> pure (V2.app (V2.I (name x) []) []) -- XXX: or add var?

    V1.Conn c es ->
      case (c,es) of
        (V1.Not, [e])         -> V2.not <$> formula e
        (V1.Implies, [e1,e2]) -> (V2.==>) <$> formula e1 <*> formula e2
        (V1.And, _) ->
           case es of
             [] -> pure V2.true
             _  -> foldr1 V2.and <$> traverse formula es
        (V1.Or, _) ->
           case es of
             [] -> pure V2.false
             _  -> foldr1 V2.or <$> traverse formula es
        (V1.Xor, _ : _)             -> foldr1 V2.xor <$> traverse formula es
        (V1.Iff, [e1,e2])           -> (V2.===) <$> formula e1 <*> formula e2
        (V1.IfThenElse, [e1,e2,e3]) -> V2.ite <$> formula e1
                                              <*> formula e2
                                              <*> formula e3
        _ -> err (text "Unsupported connective:" <+> V1.pp form)

    V1.Quant q bs f -> V2.Quant (quant q) (map binder bs) <$> formula f
    V1.Let x t f    -> mkLet <$> term t <*> formula f
      where mkLet e = V2.Let [ V2.Defn (name x) e ]
    V1.FLet x f1 f2 ->  mkLet <$> formula f1 <*> formula f2
      where mkLet e = V2.Let [ V2.Defn (name x) e ]
    V1.FAnnot t a   -> V2.Annot <$> formula t <*> traverse annot a

annot :: V1.Annot -> Trans V2.Attr
annot x = case V1.attrVal x of
            Nothing -> pure V2.Attr { V2.attrName = name (V1.attrName x)
                                    , V2.attrVal = Nothing
                                    }
            _ -> err (text "Unsupported annotation:" <+> V1.pp x)


command :: V1.Command -> Trans [V2.Command]
command com =
  case com of

    V1.CmdLogic l      -> one . V2.CmdSetLogic <$> simpleIdent l

    V1.CmdAssumption f -> one . V2.CmdAssert <$> formula f

    V1.CmdFormula f    -> one . V2.CmdAssert <$> formula f

    V1.CmdStatus s ->
      case s of
        V1.Sat -> pure [ V2.CmdCheckSat ]
        _      -> err (text "Unsupported command:" <+> V1.pp com)

    V1.CmdExtraSorts xs -> map decl <$> traverse simpleIdent xs
      where decl x = V2.CmdDeclareType x 0

    V1.CmdExtraFuns fs -> traverse decl fs
      where decl f = case V1.funAnnots f of
                       [] -> V2.CmdDeclareFun
                               <$> simpleIdent (V1.funName f)
                               <*> pure (map sort (V1.funArgs f))
                               <*> pure (sort (V1.funRes f))
                       _ -> err (text "Annotation in function declaration" <+>
                                  V1.pp com)

    V1.CmdExtraPreds fs -> traverse decl fs
      where decl f = case V1.predAnnots f of
                       [] -> V2.CmdDeclareFun
                               <$> simpleIdent (V1.predName f)
                               <*> pure (map sort (V1.predArgs f))
                               <*> pure V2.tBool
                       _ -> err (text "Annotation in predicate declaration" <+>
                                  V1.pp com)

    -- XXX: For now, we just ignore comments
    V1.CmdNotes {}    -> pure []

    V1.CmdAnnot a     -> one . V2.CmdSetInfo <$> annot a

  where one x = [x]

        simpleIdent (V1.I x []) = pure (name x)
        simpleIdent d =
          err (text "Unsupported identifier in command:" <+> V1.pp d)


script :: V1.Script -> Trans V2.Script
script s = V2.Script . concat <$> traverse command (V1.scrCommands s)




