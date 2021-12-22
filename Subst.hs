{-# LANGUAGE LambdaCase #-}
module Subst
  (
    TypeSubst,
    substExpr,
    substType
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Expr

type TypeSubst = Map Var Type

substExpr :: TypeSubst -> Expr -> Expr
substExpr sub = \case
  Var  x     -> Var x
  Lam  x t e -> Lam x (substType sub t) (substExpr sub e)
  TLam a e   -> TLam a (substExpr sub e)
  App  e e'  -> App (substExpr sub e) (substExpr sub e')
  TApp e t   -> TApp (substExpr sub e) (substType sub t)

substType :: TypeSubst -> Type -> Type
substType sub = \case
  TyVar a | Just t <- Map.lookup a sub -> t
          | otherwise                  -> TyVar a
  TyFun t t'                           -> TyFun (substType sub t) (substType sub t')
  TyPoly a t | Map.notMember a sub     -> TyPoly a (substType sub t)
             | otherwise               -> TyPoly a t
