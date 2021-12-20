{-# LANGUAGE LambdaCase #-}
module Check
  (
    CheckError(..),
    Env, TypeEnv,
    checkExpr,
    checkType
  ) where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Expr

data CheckError = UnboundVar Var
                | UnboundTyVar Var
                | AppError  (Expr, Type) (Expr, Type)
                | TAppError (Expr, Type) Type

type Env = Map.Map Var Type

type TypeEnv = Set.Set Var

subst :: Env -> Type -> Type
subst env = \case
  TyVar a | Just t <- Map.lookup a env -> t
          | otherwise                  -> TyVar a
  TyFun t t'                           -> TyFun (subst env t) (subst env t')
  TyPoly a t | Map.notMember a env     -> TyPoly a (subst env t)
             | otherwise               -> TyPoly a t

checkExpr :: Env -> TypeEnv -> Expr -> Either CheckError Type
checkExpr env _ (Var x) | Just t <- Map.lookup x env = Right t
                        | otherwise                  = Left $ UnboundVar x

checkExpr env tenv (Lam x t e) = case checkType tenv t of
  Just e  -> Left e
  Nothing -> do
    t' <- checkExpr (Map.insert x t env) tenv e
    return $ TyFun t t'

checkExpr env tenv (TLam a e) = do
  t <- checkExpr env (Set.insert a tenv) e
  return $ TyPoly a t

checkExpr env tenv (App e e') = do
  t  <- checkExpr env tenv e
  t' <- checkExpr env tenv e'
  case t of
    TyFun u u' | t' == u -> return u'
    _ -> Left $ AppError (e, t) (e', t')

checkExpr env tenv (TApp e t) | Just e <- checkType tenv t = Left e
                              | otherwise                  = do
  t' <- checkExpr env tenv e
  case t' of
    TyPoly a u -> return $ subst (Map.singleton a t) u
    _ -> Left $ TAppError (e, t') t

checkType :: TypeEnv -> Type -> Maybe CheckError
checkType tenv (TyVar a) | Set.member a tenv = Nothing
                         | otherwise         = Just $ UnboundTyVar a

checkType tenv (TyFun t t') = case checkType tenv t of
  Nothing -> checkType tenv t'
  Just e  -> Just e

checkType tenv (TyPoly a t) = checkType (Set.insert a tenv) t
