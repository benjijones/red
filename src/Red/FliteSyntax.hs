module Red.FliteSyntax where

type Prog = [Decl]

data Decl = Func { funcName :: Id
                 , funcArgs :: [Pat]
                 , funcRhs  :: Exp }
          | Other String
  deriving Show

type Id = String

data Exp = App Exp [Exp]
         | Case Exp [Alt]
         | Let [Binding] Exp
         | Var Id
         | Con Id
         | Fun Id
         | Int Int
         | Wld -- Wildcard '_'

           -- The following may be introduced by various transformations,
           -- but not by the parser.
         | Bottom
         | Alts [Id] Int
         | Ctr Id Int Int
         | Lam [Id] Exp

           -- For speculative evaluation of primitive redexes.
         | PrimApp Id [Exp]
         | Prim Id
  deriving (Eq, Show)

type Pat = Exp

type Alt = (Pat, Exp)

type Binding = (Id, Exp)

type App = [Exp]