{-# LANGUAGE CPP #-}
module Expr
  (
    Var,
    Expr(..),
    TopExpr(..),
    Type(..),
    Pretty, pretty, pprint
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Tuple

type Var = String

{-
e = x         variable
  | λx:τ. e   abstraction
  | Λα. e     type abstraction
  | e e'      application
  | e [τ]     type application
-}
data Expr = Var Var
          | Lam Var Type Expr
          | TLam Var Expr
          | App Expr Expr
          | TApp Expr Type
          deriving (Show, Eq)

data TopExpr = Expr Expr
             | Bind Var Expr
             deriving (Show, Eq)

{-
τ = α         type variable
  | τ → τ'    type function
  | ∀α. τ     universal quantifier
-}
data Type = TyVar Var
          | TyFun Type Type
          | TyPoly Var Type
          deriving Show

class Pretty a where
  pretty :: a -> String

pprint :: Pretty a => a -> IO ()
pprint = putStrLn . pretty

instance Pretty Expr where
  pretty (Var x)      = x
  -- FIXME: Consecutive abstractions still needs to specify the type of each
  -- bound variables, so concatenation is less obvious in this case
  pretty (Lam x t e)  = "λ" ++ x ++ ":" ++ pretty t ++ ". " ++ pretty e
  pretty (TLam a e)   = "Λ" ++ a ++ help e
    where
      -- NOTE: Consecutive type abstractions can be be packed together
      -- easily by concatenating their bound variables
#ifdef PACK_TLAM
      help (TLam a e) = " " ++ a ++ help e
#endif
      help e          = ". " ++ pretty e

  pretty (App e e')   = help (App e e')
    where
      help (App e e') = help e ++ " " ++ help' e'
      help e          = help' e

      help' (Var x)   = x
      help' e         = "(" ++ pretty e ++ ")"
  pretty (TApp e t)   = help e ++ " [" ++ pretty t ++ "]"
    where
      help (TApp e t) = pretty (TApp e t)
      help (Var x)    = x
      help e          = "(" ++ pretty e ++ ")"

instance Pretty TopExpr where
  pretty (Expr e)   = pretty e
  pretty (Bind x e) = x ++ " = " ++ pretty e

instance Pretty Type where
  pretty (TyVar a)       = a
  pretty (TyFun t t')    = help t ++ " → " ++ help' t'
    where
      help (TyVar a)     = a
      help t             = "(" ++ pretty t ++ ")"

      help' (TyPoly a t) = "(" ++ pretty (TyPoly a t) ++ ")"
      help' t            = pretty t
  pretty (TyPoly a t)    = "∀" ++ help t a
    where
      -- NOTE: Consecutive universal quantifiers can be packed in the same
      -- quantifier, however if they quantify over the same type variable
      -- they shouldn't be packed together
#ifdef PACK_FORALL
      help (TyPoly a t) acc | isSuffixOf a acc = acc ++ ". " ++ pretty (TyPoly a t)
                            | otherwise        = help t $ acc ++ " " ++ a
#endif
      help t            acc                    = acc ++ ". " ++ pretty t

instance Eq Type where
  (==) = go Map.empty
    where
      go env (TyVar a) (TyVar b) | Just c <- Map.lookup a env = b == c
                                 | Just _ <- Map.lookup b env = False
                                 | otherwise                  = a == b
      go env (TyFun a b) (TyFun c d)                          = go env a c && go env b d
      go env (TyPoly a t) (TyPoly b t') | a == b              = go env t t'
                                        | otherwise           = go (Map.insert a b env) t t'
      go env _ _                                              = False
