module Parse
  (
    parseExpr,
    parseType,
    parseExprTop
  ) where

import Expr
import Comb

parseExpr :: Parser Expr
parseExpr = do
  e <- pBetween pSpaces pSpaces p
  es <- pMany p'
  return $ foldl App e es
  where
    p = pChoice "expression"
      [ pParens parseExpr
      , parseLam
      , parseTLam
      , parseType'
      , parseVar ]

    p' = pChoice "argument"
      [ pParens parseExpr
      , parseType'
      , parseVar ]

parseVar :: Parser Expr
parseVar = Var <$> pIdent <* pSpaces

parseType' :: Parser Expr
parseType' = do
  pSymbol "["
  t <- parseType
  pSymbol "]"
  return $ Type t

parseLam :: Parser Expr
parseLam = do
  pSymbol "\\" <|> pSymbol "λ"
  x <- pIdent <* pSpaces
  pSymbol ":"
  t <- parseType
  pSymbol "."
  e <- parseExpr
  return $ Lam x t e

parseTLam :: Parser Expr
parseTLam = do
  pSymbol "/\\" <|> pSymbol "Λ"
  a <- pIdent <* pSpaces
  pSymbol "."
  e <- parseExpr
  return $ TLam a e

parseType :: Parser Type
parseType = do
  t <- pBetween pSpaces pSpaces p
  pTry $ parseTyFun t <|> return t
  where
    p = pChoice "type"
      [ pParens parseType
      , parseTyPoly
      , parseTyVar ]

parseTyVar :: Parser Type
parseTyVar = TyVar <$> pIdent <* pSpaces

parseTyFun :: Type -> Parser Type
parseTyFun t = do
  pSymbol "->" <|> pSymbol "→"
  t' <- parseType
  return $ TyFun t t'

parseTyPoly :: Parser Type
parseTyPoly = do
  pSymbol "forall" <|> pSymbol "∀"
  a <- pIdent <* pSpaces
  pSymbol "."
  t <- parseType
  return $ TyPoly a t

parseExprTop :: Parser (Maybe Expr)
parseExprTop = pChoice "top"
  [ Just <$> parseExpr
  , Nothing <$ pSpaces ]
