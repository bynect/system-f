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
putError (UnboundVar x)             = putStrLn $ "• Variable not bound `" ++ x ++ "`"
putError (UnboundTyVar a)           = putStrLn $ "• Type variable not bound `" ++ a ++ "`"
putError (AppError (e, t) (e', t')) = do
  putStrLn $ "• Invalid application"
  putStrLn $ "• Expression of type `" ++ pretty t ++ "`"
  putStrLn $ "    " ++ pretty e
  putStrLn $ "  was applied to expression of type `" ++ pretty t' ++ "`"
  putStrLn $ "    " ++ pretty e'

putError (TAppError (e, t) t') = do
  putStrLn $ "• Invalid type application"
  putStrLn $ "• Expression of type `" ++ pretty t ++ "`"
  putStrLn $ "    " ++ pretty e
  putStrLn $ "  was applied to type `" ++ pretty t' ++ "`"

handleExpr :: Env -> Expr -> (Type -> IO ()) -> IO ()
handleExpr env e f = case checkExpr env e of
  Right t -> f t
  Left  e -> do
    putStrLn "⊥"
    putStrLn ""
    putError e

handleTopExpr :: IORef Env -> Maybe TopExpr -> IO ()
handleTopExpr env = \case
  Just a  -> do
    env'  <- readIORef env
    pprint a
    case a of
      Expr e   -> handleExpr env' e pprint
      Bind x e -> handleExpr env' e $ \t -> do
        putStr $ x ++ " : "
        pprint t
        modifyIORef env $ Map.insert x t
  Nothing -> putStrLn ""

handleFile :: IORef Env -> String -> IO ()
handleFile env f = error "Not implemented yet"

handleLoop :: IORef Env -> IO ()
handleLoop env = do
  putStr "c> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn ""
    else do
      s <- getLine
      case pRun parseTopExpr s of
        Right a -> handleTopExpr env a
        Left e  -> print e
      handleLoop env

main :: IO ()
main = do
  env <- newIORef $ Map.empty
  args <- getArgs
  case args of
    f:_ -> handleFile env f
    []  -> handleLoop env
