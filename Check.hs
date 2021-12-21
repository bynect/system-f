{-# LANGUAGE LambdaCase #-}
module Check
  (
    Env,
    CheckError(..),
    checkExpr,
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

type FtvSet = Set.Set Var

ftv :: FtvSet -> Type -> FtvSet
ftv env (TyVar a) | a `Set.member` env = Set.empty
                  | otherwise          = Set.singleton a
ftv env (TyFun t t') = ftv env t `Set.union` ftv env t'
ftv env (TyPoly a t) = ftv (Set.insert a env) t

substVar :: Var -> Var -> Type -> Type
substVar b c = \case
  TyVar a    -> TyVar $ if a == b then c else a
  TyFun t t' -> TyFun (substVar b c t) (substVar b c t')
  TyPoly a t -> TyPoly a $ if a == b then t else substVar b c t

-- TODO: Is this useful?
substType :: Env -> Type -> Type
substType env = \case
  TyVar a | Just t <- Map.lookup a env -> t
          | otherwise                  -> TyVar a
  TyFun t t'                           -> TyFun (substType env t) (substType env t')
  TyPoly a t | Map.notMember a env     -> TyPoly a (substType env t)
             | otherwise               -> TyPoly a t

unique :: FtvSet -> Var
unique ftvs = go 0
  where
    go n = let t = "t" ++ show n in
      if t `Set.member` ftvs
        then go (n + 1)
        else t

checkExpr :: Env -> Expr -> Either CheckError Type
checkExpr env (Var x) | Just t <- Map.lookup x env = Right t
                      | otherwise                  = Left $ UnboundVar x
checkExpr env (Lam x t e) = TyFun t  <$> checkExpr (Map.insert x t env) e
checkExpr env (TLam a e)  = TyPoly a <$> checkExpr env e
checkExpr env (App e e')  = do
  t  <- checkExpr env e
  t' <- checkExpr env e'
  case t of
    TyFun u u' | t' == u -> return u'
    _                    -> Left $ AppError (e, t) (e', t')
checkExpr env (TApp e t)  = do
  t' <- checkExpr env e
  case t' of
    TyPoly a u -> return $ help u
      where
        -- NOTE: Handle name conflicts with free type variables
          help (TyVar b) | a == b                 = t
                         | otherwise              = TyVar b
          help (TyFun t t')                       = TyFun (help t) (help t')
          help (TyPoly b t) | a == b              = TyPoly b t
                            | b `Set.member` ftvs = let c = unique ftvs in TyPoly c $ help $ substVar b c t
                            | otherwise           = TyPoly b (help t)
            where
              ftvs = ftv Set.empty t
    _          -> Left $ TAppError (e, t') t
