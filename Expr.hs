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

data Expr = Var Var
          | Lam Var Type Expr
          | App Expr Expr
          deriving (Show, Eq)

data Type = TyVar Var
          | TyFun Type Type
          deriving (Show, Eq)

class Pretty a where
  pretty :: a -> String

pprint :: Pretty a => a -> IO ()
pprint = putStrLn . pretty

instance Pretty Expr where
  pretty (Var x)     = x
  pretty (Lam x t e) = "λ" ++ x ++ ":" ++ pretty t ++ ". " ++ pretty e
  pretty (App e e')  = help e ++ " " ++ help e'
    where
      help (Var x) = x
      help e       = pretty e

instance Pretty Type where
  pretty (TyVar a)    = a
  pretty (TyFun t t') = help t ++ " -> " ++ pretty t'
    where
      help (TyVar a) = a
      help t         = "(" ++ pretty t ++ ")"
