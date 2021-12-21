{-# LANGUAGE CPP #-}
module Expr
  (
    Var, ExprEnv, TypeEnv,
    Expr(..), TopExpr(..),
    Type(..),
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Tuple

{-
x
-}
type Var = String

{-
Γ = ɛ         empty
  | Γ, x:τ    assumptions
-}
type ExprEnv = Map.Map Var Type

{-
Θ = ɛ         empty
  | Θ, α      type assumptions
-}
type TypeEnv = Set.Set Var

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
          deriving Eq

data TopExpr = Expr Expr
             | Bind Var Expr
             deriving Eq

{-
τ = α         type variable
  | τ → τ'    type function
  | ∀α. τ     universal quantifier
-}
data Type = TyVar Var
          | TyFun Type Type
          | TyPoly Var Type

#ifdef SHOW_UNICODE
#define SHOW_LAM     "λ"
#define SHOW_TLAM    "Λ"
#define SHOW_ARROW   "→"
#define SHOW_FORALL  "∀"
#define SHOW_ENV     "Γ"
#define SHOW_TENV    "Θ"
#else
#define SHOW_LAM     "\\"
#define SHOW_TLAM    "/\\"
#define SHOW_ARROW   "->"
#define SHOW_FORALL  "forall "
#define SHOW_ENV     "E"
#define SHOW_TENV    "TE"
#endif

instance Show Expr where
  show (Var x)        = x
  -- FIXME: Consecutive abstractions still needs to specify the type of each
  -- bound variables, so concatenation is less obvious in this case
  show (Lam x t e)    = SHOW_LAM  ++ x ++ ":" ++ show t ++ ". " ++ show e
  show (TLam a e)     = SHOW_TLAM ++ a ++ help e
    where
#ifdef PACK_TLAM
      -- NOTE: Consecutive type abstractions can be be packed together
      -- easily by concatenating their bound variables
      help (TLam a e) = " " ++ a ++ help e
#endif
      help e          = ". " ++ show e

  show (App e e')     = help (App e e')
    where
      help (App e e') = help e ++ " " ++ help' e'
      help e          = help' e

      help' (Var x)   = x
      help' e         = "(" ++ show e ++ ")"
  show (TApp e t)     = help e ++ " [" ++ show t ++ "]"
    where
      help (TApp e t) = show (TApp e t)
      help (Var x)    = x
      help e          = "(" ++ show e ++ ")"

instance Show TopExpr where
  show (Expr e)   = show e
  show (Bind x e) = x ++ " = " ++ show e

instance Show Type where
  show (TyVar a)         = a
  show (TyFun t t')      = help t ++ arrow ++ help' t'
    where
      arrow              = " " ++ SHOW_ARROW ++ " "

      help (TyVar a)     = a
      help t             = "(" ++ show t ++ ")"

      help' (TyPoly a t) = "(" ++ show (TyPoly a t) ++ ")"
      help' t            = show t
  show (TyPoly a t)      = SHOW_FORALL ++ help t a
    where
#ifdef PACK_FORALL
      -- NOTE: Consecutive universal quantifiers can be packed in the same
      -- quantifier, however if they quantify over the same type variable
      -- they shouldn't be packed together
      help (TyPoly a t) acc | isSuffixOf a acc = acc ++ ". " ++ show (TyPoly a t)
                            | otherwise        = help t $ acc ++ " " ++ a
#endif
      help t            acc                    = acc ++ ". " ++ show t

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
