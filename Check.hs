module Check
  (
    Env,
    TypeEnv,
    checkExpr,
    checkType
  ) where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Expr

type Env = Map.Map Var Type

type TypeEnv = Set.Set Var

checkExpr :: Env -> TypeEnv -> Expr -> Either String Type
checkExpr env tenv (Var x) | Just t <- Map.lookup x env = Right t
                           | otherwise                  = Left $ "Unbound variable " ++ x

checkExpr env tenv (Lam x t e) = case checkType tenv t of
  Just e -> Left e
  Nothing -> do
    t' <- checkExpr (Map.insert x t env) tenv e
    return $ TyFun t t'

checkExpr env tenv (App e e') = do
  t  <- checkExpr env tenv e
  t' <- checkExpr env tenv e'
  case t of
    TyFun u u' | t' == u -> return u'
    _ -> throwError $ "Expected " ++ pretty (TyFun t' $ TyVar "a") ++ " instead of " ++ pretty t

checkType :: TypeEnv -> Type -> Maybe String
checkType tenv (TyVar a) | Set.member a tenv = Nothing
                         | otherwise         = Just $ "Unbound type variable " ++ a

checkType tenv (TyFun t t') = case checkType tenv t of
  Nothing -> checkType tenv t'
  Just e  -> Just e
