{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO
import System.Environment
import Data.IORef
import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Check
import Parse
import Expr
import Comb

putIndent :: String -> IO ()
putIndent s = putStr "  " >> putStrLn s

putError :: CheckError -> IO ()
putError (UnboundVar x)             = do
  putStrLn $ "Unbound variable"
  putIndent x
putError (UnboundTyVar a)           = do
  putStrLn $ "Unbound type variable"
  putIndent a
putError (AppError (e, t) (e', t')) = do
  putStrLn "Failed application of"
  putIndent $ pretty e ++ "  :  " ++ pretty t
  putStrLn "with"
  putIndent $ pretty e ++ "  :  " ++ pretty t
putError (TAppError (e, t) t') = do
  putStrLn "Failed type application of"
  putIndent $ pretty e ++ "  :  " ++ pretty t
  putStrLn "with type"
  putIndent $ pretty t'

runExpr :: Env -> TypeEnv -> Expr -> (Type -> IO ()) -> IO ()
runExpr env tenv e f = case checkExpr env tenv e of
  Right t -> f t
  Left e  -> putStrLn "⊥\n" >> putError e

runTop :: IORef Env -> IORef TypeEnv -> String -> IO ()
runTop env tenv s = case pRun parseTopExpr s of
  Right (Just a) -> do
    env'  <- readIORef env
    tenv' <- readIORef tenv
    pprint a
    case a of
      Expr e   -> runExpr env' tenv' e pprint
      Bind x e -> runExpr env' tenv' e $ \t -> do
        putStr $ x ++ " : "
        pprint t
        modifyIORef env $ Map.insert x t
  Right Nothing  -> putStrLn ""
  Left e         -> print e

loop :: IORef Env -> IORef TypeEnv -> IO ()
loop env tenv = do
  putStr "c> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn ""
    else do
      s <- getLine
      runTop env tenv s
      loop env tenv

main :: IO ()
main = do
  env  <- newIORef $ Map.empty
  tenv <- newIORef $ Set.fromList
    [ "Int"
    , "Bool" ]
  loop env tenv
