module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (hFlush, isEOF, stdout)
import Check
import Parse
import Expr
import Comb

run :: Env -> TypeEnv -> String -> IO ()
run env tenv s = case pRun parseTopExpr s of
  Right (Just a) -> do
    pprint a
    case checkExpr env tenv $ unTopExpr a of
      Right t -> pprint t
      Left e -> putStrLn e
  Right Nothing -> return ()
  Left e -> print e

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  case eof of
    True -> putStrLn ""
    False -> do
      s <- getLine
      run env tenv s
      main
  where
    env  = Map.empty
    tenv = Set.fromList
      [ "Int"
      , "Bool" ]
