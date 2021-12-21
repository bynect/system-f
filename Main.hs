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

handleExpr :: ExprEnv -> TypeEnv -> Expr -> (Type -> IO ()) -> IO ()
handleExpr env tenv e f = case checkExpr env tenv e of
  Right t -> f t
  Left  e -> putStr ['⊥', '\n', '\n'] >> print e

handleTopExpr :: IORef ExprEnv -> TypeEnv -> Maybe TopExpr -> IO ()
handleTopExpr env tenv (Just a) = do
    env'  <- readIORef env
    print a
    case a of
      Expr e   -> handleExpr env' tenv e print
      Bind x e -> handleExpr env' tenv e $ \t -> do
        putStr $ x ++ " : "
        print t
        modifyIORef env $ Map.insert x t
handleTopExpr _   _    Nothing  = putStrLn ""

handleFile :: IORef ExprEnv -> String -> IO ()
handleFile env f = error "Not implemented yet"

handleLoop :: IORef ExprEnv -> IO ()
handleLoop env = do
  putStr "c> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn ""
    else do
      s <- getLine
      case pRun parseTopExpr s of
        Right a -> handleTopExpr env tenv a
        Left e  -> print e
      handleLoop env
  where
    tenv = Set.fromList
      [ "Nat"
      , "Bool" ]

main :: IO ()
main = do
  env  <- newIORef $ Map.empty
  args <- getArgs
  case args of
    f:_ -> handleFile env f
    []  -> handleLoop env
