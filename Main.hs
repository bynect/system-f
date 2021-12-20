module Main where

import System.IO (hFlush, isEOF, stdout)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Check
import Parse
import Expr
import Comb

runExpr :: Env -> TypeEnv -> Expr -> (Type -> IO ()) -> IO ()
runExpr env tenv e f = case checkExpr env tenv e of
  Right t -> f t
  Left e  -> putStrLn e

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
