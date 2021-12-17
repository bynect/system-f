module Main where

import System.IO (hFlush, isEOF, stdout)
import Check
import Parse
import Expr
import Comb

run :: String -> IO ()
run s = case pRun parseExpr s of
  Right a -> pprint a
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
      run s
      main
