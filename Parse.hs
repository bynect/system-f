module Parse
  (
    parseExpr,
    parseType,
    parseExprTop
  ) where

import Expr
import Comb

{-
expr ::= x
       | \x : type . expr
       | expr expr'
-}
parseExpr :: Parser Expr
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

parseVar :: Parser Expr
parseVar = Var <$> pIdent <* pSpaces

parseLam :: Parser Expr
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
parseType :: Parser Type
parseType = do
  t <- pBetween pSpaces pSpaces p
  pTry $ parseTyFun t <|> return t
  where
    p = pChoice "type"
      [ pParens parseType
      , parseTyVar ]

parseTyVar :: Parser Type
parseTyVar = TyVar <$> pIdent <* pSpaces

parseTyFun :: Type -> Parser Type
parseTyFun t = do
  pSymbol "->"
  t' <- parseType
  return $ TyFun t t'

parseExprTop :: Parser (Maybe Expr)
parseExprTop = pChoice "top"
  [ Just <$> parseExpr
  , Nothing <$ pSpaces ]
