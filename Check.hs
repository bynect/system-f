{-# LANGUAGE LambdaCase #-}
module Check
  (
    CheckError(..),
    checkExpr,
    checkType
  ) where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Expr

data CheckError = UnboundVar   Var
                | UnboundTyVar Var
                | ApplyFail  (Expr, Type) (Expr, Type) (Maybe Type)
                | TApplyFail (Expr, Type) Type

instance Show CheckError where
  show (UnboundVar x)                = "• Variable not bound `" ++ x ++ "`"
  show (UnboundTyVar a)              = "• Type variable not bound `" ++ a ++ "`"
  show (ApplyFail (e, t) (e', t') u) = intercalate "\n"
    [ "• Invalid application" ++ help u
    , "• Expression of type `" ++ show t ++ "`"
    , "    " ++ show e
    , "  was applied to expression of type `" ++ show t' ++ "`"
    , "    " ++ show e' ]
    where
      help (Just t) = ", expected type `" ++ show t ++ "`"
      help Nothing  = ""
  show (TApplyFail (e, t) t')        = intercalate "\n"
    [ "• Invalid type application"
    , "• Expression of type `" ++ show t ++ "`"
    , "    " ++ show e
    , "  was applied to type `" ++ show t' ++ "`" ]

substType :: (Var, Type) -> Type -> Type
substType s@(b, t) = \case
  TyVar a | a == b       -> t
          | otherwise    -> TyVar a
  TyFun t t'             -> TyFun (substType s t) (substType s t')
  TyPoly a t | a /= b    -> TyPoly a (substType s t)
             | otherwise -> TyPoly a t

checkExpr :: ExprEnv -> TypeEnv -> Expr -> Either CheckError Type
checkExpr env tenv = \case
  Var x | Just t <- Map.lookup x env -> Right t
        | otherwise                  -> Left $ UnboundVar x
  Lam x t e                          -> do
    _ <- checkType tenv t
    TyFun t  <$> checkExpr (Map.insert x t env) tenv e
  TLam a e                           -> TyPoly a <$> checkExpr env (Set.insert a tenv) e
  App e e'                           -> do
    t  <- checkExpr env tenv e
    t' <- checkExpr env tenv e'
    case t of
      TyFun u u' | t' == u           -> return u'
                 | otherwise         -> Left $ ApplyFail (e, t) (e', t') $ Just $ TyFun u u'
      _                              -> Left $ ApplyFail (e, t) (e', t') Nothing
  TApp e t'                          -> do
    _ <- checkType tenv t'
    t <- checkExpr env tenv e
    case t of
      TyPoly a u                     -> Right $ substType (a, t') u
      _                              -> Left $ TApplyFail (e, t) t'

checkType :: TypeEnv -> Type -> Either CheckError ()
checkType tenv = \case
  TyVar a | a `Set.member` tenv -> Right ()
          | otherwise           -> Left $ UnboundTyVar a
  TyFun t t'                    -> checkType tenv t >> checkType tenv t'
  TyPoly a t                    -> checkType (Set.insert a tenv) t
