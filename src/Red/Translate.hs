module Red.Translate where

import Red.Syntax

import CoreSyn (CoreProgram, CoreBind, Bind (NonRec, Rec))

translate :: CoreProgram -> RedProgram
translate p = map translateBind p

translateBind :: CoreBind -> Template
translateBind (NonRec b expr) = undefined
translateBind (Rec exprs)     = undefined