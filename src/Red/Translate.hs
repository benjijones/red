module Red.Translate where

import qualified Flite.Syntax as Flite

import CoreSyn (Alt, AltCon (..), Bind (..), CoreProgram, CoreBind, Expr (..))
import Var (Var, Id, varName)
import Unique (getUnique)
import Literal (Literal (LitInteger))
import Unique (Uniquable)
import TypeRep (Type (..))

translate :: CoreProgram -> Flite.Prog
translate = map translateBind

translateBind :: CoreBind -> Flite.Decl
translateBind (NonRec var expr) = Flite.Func { Flite.funcName = show . getUnique $ var
                                             , Flite.funcArgs = []
                                             , Flite.funcRhs  = translateExpr expr }
translateBind (Rec exprs)     = undefined

translateExpr :: Expr Var -> Flite.Exp
translateExpr (Var id)        = Flite.Var . uniqueName $ id
translateExpr (Lit lit)       = translateLit lit
translateExpr (App func arg)  = Flite.App (translateExpr func) [(translateExpr arg)]
translateExpr (Lam id body)   = Flite.Lam [uniqueName $ id] $ translateExpr body
translateExpr (Let bind body) = let decl = translateBind bind
                                in Flite.Let [(Flite.funcName decl, Flite.funcRhs decl)] $ translateExpr body
translateExpr (Case expr id ty alts)
                              = Flite.Case (translateExpr expr) (map translateAlt alts)
translateExpr (Cast expr _coercion)
                              = Flite.Var "unknown cast"
translateExpr (Tick _id _expr) = Flite.Var "unknown tick"
translateExpr (Type ty)    = translateType ty
translateExpr (Coercion _coercion)
                              = Flite.Var "unknown coercion"
                              
translateLit :: Literal -> Flite.Exp
translateLit (LitInteger n _) = Flite.Int . fromInteger $ n
translateLit _ = Flite.Var "unknown literal"

translateAlt :: Alt Var -> Flite.Alt
translateAlt (DataAlt dataCon, vars, expr) = (Flite.Con $ uniqueName dataCon, translateExpr expr)
translateAlt (LitAlt _literal, _vars, _expr)  = error "LitAlt encountered in translateAlt"

translateType :: Type -> Flite.Exp
translateType (TyVarTy var) = Flite.Var $ "TyVarTy " ++ uniqueName var
translateType (AppTy ty1 ty2) = Flite.Var $ "AppTy " ++ "ty1" ++ " " ++ "ty2"
translateType (TyConApp tycon args) = Flite.Var $ "TyConApp"
translateType (FunTy ty1 ty2) = Flite.Var $ "FunTy"


uniqueName :: (Uniquable a) => a -> String
uniqueName = show . getUnique
