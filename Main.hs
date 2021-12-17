module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (hFlush, isEOF, stdout)
import Check
import Parse
import Expr
import Comb

run :: TypeEnv -> String -> IO ()
run env s = case pRun parseExpr s of
  Right a -> do
    pprint a
    case checkExpr env a of
      Right t -> pprint t
      Left e -> putStrLn e
  Left e -> print e

main :: IO ()
main = do
  let env = Map.empty
  putStr "> "
  hFlush stdout
  eof <- isEOF
  case eof of
    True -> putStrLn ""
    False -> do
      s <- getLine
      run env s
      main
