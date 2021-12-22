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

handleExpr :: ExprEnv -> TypeEnv -> Expr -> (Type -> IO ()) -> IO ()
handleExpr env tenv e f = case checkExpr env tenv e of
  Right t -> f t
  Left  e -> putStr ['⊥', '\n', '\n'] >> print e

handleTopExpr :: IORef ExprEnv -> TypeEnv -> TopExpr -> IO ()
handleTopExpr env tenv a = do
    env'  <- readIORef env
    print a
    case a of
      Expr e   -> handleExpr env' tenv e print
      Bind x e -> handleExpr env' tenv e $ \t -> do
        putStr $ x ++ " : "
        print t
        modifyIORef env $ Map.insert x t

handleLoop :: IORef ExprEnv -> TypeEnv -> IO ()
handleLoop env tenv = do
  putStr "c> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn ""
    else do
      s <- getLine
      case pRun parseTopExpr s of
        Right (Just a) -> handleTopExpr env tenv a
        Right Nothing  -> putStrLn ""
        Left e         -> print e >> putStrLn s
      handleLoop env tenv

handleFile :: IORef ExprEnv -> TypeEnv -> String -> IO ()
handleFile env tenv f = do
  s <- readFile f
  case pRun parseTopExpr' s of
    Right a -> forM_ a $ handleTopExpr env tenv
    Left e  -> print e

handleFiles :: IORef ExprEnv -> TypeEnv -> [String] -> IO ()
handleFiles env tenv = flip forM_ $ \f ->
  if f == "-"
     then handleLoop env tenv
     else handleFile env tenv f

main :: IO ()
main = do
  env  <- newIORef $ Map.empty
  args <- getArgs
  case args of
    [] -> handleLoop env tenv
    fs -> handleFiles env tenv fs
  where
    tenv = Set.fromList
      [ "Nat"
      , "Bool" ]
