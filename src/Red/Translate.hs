module Red.Translate where

import qualified Red.FliteSyntax as Flite

import CoreSyn (Alt, AltCon (..), Bind (..), CoreProgram, CoreBind, Expr (..))
import Var (Var, Id, varName)
import Unique (getUnique)
import Literal (Literal (LitInteger))

translate :: CoreProgram -> Flite.Prog
translate = map translateBind

translateBind :: CoreBind -> Flite.Decl
translateBind (NonRec var expr) = Flite.Func { Flite.funcName = show . getUnique $ var
                                             , Flite.funcArgs = []
                                             , Flite.funcRhs  = translateExpr expr }
translateBind (Rec exprs)     = undefined

translateExpr :: Expr Var -> Flite.Exp
translateExpr (Var id)        = Flite.Var . show . getUnique $ id
translateExpr (Lit lit)       = translateLit lit
translateExpr (App func arg)  = Flite.App (translateExpr func) [(translateExpr arg)]
translateExpr (Lam id body)   = Flite.Lam [show . getUnique $ id] $ translateExpr body
translateExpr (Let bind body) = let decl = translateBind bind
                                in Flite.Let [(Flite.funcName decl, Flite.funcRhs decl)] $ translateExpr body
--translateExpr (Case expr id ty alts)
--                              = Flite.Case (translateExpr expr) (map translateAlt alts)
translateExpr _               = Flite.Var "unknown expression"

translateLit :: Literal -> Flite.Exp
translateLit (LitInteger n _) = Flite.Int . fromInteger $ n
translateLit _ = Flite.Var "unknown literal"

-- WIP
--translateAlt :: Alt Var -> Flite.Alt
--translateAlt (DataAlt  )