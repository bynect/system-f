module Check
  (
    TypeEnv,
    checkExpr,
    checkType
  ) where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Expr

type TypeEnv = Map.Map Var Type

checkExpr :: TypeEnv -> Expr -> Either String Type
checkExpr env (Var x) | Just t <- Map.lookup x env = Right t
                      | otherwise                  = Left $ "Unbound variable " ++ x

checkExpr env (Lam x t e) = do
  t' <- checkExpr (Map.insert x t env) e
  return $ TyFun t t'

checkExpr env (App e e') = do
  t  <- checkExpr env e
  t' <- checkExpr env e'
  case t of
    TyFun u u' | t' == u -> return u'
    _ -> throwError $ "Expected " ++ pretty (TyFun t' $ TyVar "a") ++ " instead of " ++ pretty t

checkType = error ""
