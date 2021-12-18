module Expr
  (
    Var,
    Expr(..),
    Type(..),
    Pretty,
    pretty,
    pprint
  ) where

type Var = String

{-
e = x
  | [τ]
  | λx:τ. e
  | Λa. e
  | e e'
-}
data Expr = Var Var
          | Type Type
          | Lam Var Type Expr
          | TLam Var Expr
          | App Expr Expr
          deriving (Show, Eq)

{-
τ = α
  | τ → τ'
  | ∀α. τ
-}
data Type = TyVar Var
          | TyFun Type Type
          | TyPoly Var Type
          deriving (Show, Eq)

class Pretty a where
  pretty :: a -> String

pprint :: Pretty a => a -> IO ()
pprint = putStrLn . pretty

instance Pretty Expr where
  pretty (Var x)     = x
  pretty (Type t)    = "[" ++ pretty t ++ "]"
  pretty (Lam x t e) = "λ" ++ x ++ ":" ++ pretty t ++ ". " ++ pretty e
  pretty (TLam a e)  = "Λ" ++ a ++ ". " ++ pretty e
  pretty (App e e')  = help (App e e')
    where
      help (App e e') = help e ++ " " ++ help' e'
      help e          = help' e

      help' (Var x)  = x
      help' (Type t) = pretty $ Type t
      help' e        = "(" ++ pretty e ++ ")"

instance Pretty Type where
  pretty (TyVar a)    = a
  pretty (TyFun t t') = help t ++ " → " ++ pretty t'
    where
      help (TyVar a) = a
      help t         = "(" ++ pretty t ++ ")"

  pretty (TyPoly a t) = "∀" ++ help t a
    where
      help (TyPoly a t) acc = help t $ acc ++ " " ++ a
      help t            acc = acc ++ ". " ++ pretty t
