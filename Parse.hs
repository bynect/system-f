module Parse
  (
    parseExpr,
    parseType
  ) where

import Expr
import Comb

{-
expr ::= x
       | \x : type . expr
       | expr expr'
-}
parseExpr = do
  e <- pBetween pSpaces pSpaces p
  es <- pMany p'
  return $ foldl App e es
  where
    p = pChoice "expression"
      [ pParens parseExpr
      , parseLam
      , parseVar ]

    p' = pChoice "argument"
      [ pParens parseExpr
      , parseVar ]

parseVar = Var <$> pIdent <* pSpaces

parseLam = do
  pSymbol "\\" <|> pSymbol "λ"
  x <- pIdent <* pSpaces
  pSymbol ":"
  t <- parseType
  pSymbol "."
  e <- parseExpr
  return $ Lam x t e

{-
type ::= a
       | type -> type'
-}
parseType = pBetween pSpaces pSpaces p
  where
    p = pChoice "type"
      [ pParens parseType
      , parseTyVar
      , parseTyFun ]

parseTyVar = TyVar <$> pIdent

parseTyFun = do
  t <- parseType
  pSymbol "->"
  t' <- parseType
  return $ TyFun t t'
