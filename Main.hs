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

run :: IORef Env -> IORef TypeEnv -> String -> IO ()
run env tenv s = case pRun parseTopExpr s of
  Right (Just a) -> do
    env'  <- readIORef env
    tenv' <- readIORef tenv
    pprint a
    case a of
      Bind x e -> case checkExpr env' tenv' e of
          Right t -> do
            putStr $ x ++ " : "
            pprint t
            modifyIORef env $ Map.insert x t
          Left e  -> putStrLn e
      Expr e -> case checkExpr env' tenv' e of
        Right t -> pprint t
        Left e  -> putStrLn e
  Right Nothing -> putStrLn ""
  Left e -> print e

loop :: IORef Env -> IORef TypeEnv -> IO ()
loop env tenv = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn ""
    else do
      s <- getLine
      run env tenv s
      loop env tenv

main :: IO ()
main = do
  env  <- newIORef $ Map.empty
  tenv <- newIORef $ Set.fromList
    [ "Int"
    , "Bool" ]
  loop env tenv
