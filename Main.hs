module Main where

import System.Environment
import System.IO
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
import Subst

handleExpr :: ExprEnv -> TypeEnv -> Expr -> (Type -> IO ()) -> IO ()
handleExpr env tenv e f = case checkExpr env tenv e of
  Right t -> f t
  Left  e -> putStr ['⊥', '\n', '\n'] >> print e

handleTopExpr :: IORef ExprEnv -> IORef TypeSubst -> TopExpr -> IO ()
handleTopExpr env sub a = do
  env' <- readIORef env
  sub' <- readIORef sub
  print a
  case a of
    Expr e     -> handleExpr env' tenv (substExpr sub' e) print
    Bind x e   -> handleExpr env' tenv (substExpr sub' e) $ \t -> do
      putStr $ x ++ " : "
      print t
      modifyIORef env $ Map.insert x t
    BindTy a t -> case checkType tenv t' of
      Right () -> do
        putStr $ a ++ " : "
        print t
        modifyIORef sub $ Map.insert a t'
      Left  e  -> print e
      where
        t' = substType sub' t
  where
    tenv = Set.empty

handleLoop :: IORef ExprEnv -> IORef TypeSubst -> IO ()
handleLoop env sub = do
  putStr "c> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn ""
    else do
      s <- getLine
      case pRun parseTopExpr s of
        Right (Just a) -> handleTopExpr env sub a
        Right Nothing  -> putStrLn ""
        Left  e        -> print e >> putStrLn s
      handleLoop env sub

handleFile :: IORef ExprEnv -> IORef TypeSubst -> String -> IO ()
handleFile env sub f = do
  s <- readFile f
  case pRun parseTopExpr' s of
    Right a -> forM_ a $ handleTopExpr env sub
    Left  e -> print e

handleFiles :: IORef ExprEnv -> IORef TypeSubst -> [String] -> IO ()
handleFiles env sub = flip forM_ $ \f ->
  if f == "-"
     then handleLoop env sub
     else handleFile env sub f

main :: IO ()
main = do
  env  <- newIORef $ Map.empty
  sub  <- newIORef $ Map.empty
  args <- getArgs
  case args of
    [] -> handleLoop  env sub
    fs -> handleFiles env sub fs
