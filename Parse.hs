module Parse
  (
    parseExpr,
    parseTopExpr,
    parseType
  ) where

import Expr
import Comb

parseIdent :: Parser String
parseIdent = pMany' $ pPred "identifier" pred
  where
      pred c = (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || c == '_'

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
parseVar = Var <$> parseIdent <* pSpaces

parseType' :: Parser Expr
parseType' = do
  pSymbol "["
  t <- parseType
  pSymbol "]"
  return $ Type t

parseLam :: Parser Expr
parseLam = do
  pSymbol "\\" <|> pSymbol "λ"
  x <- parseIdent <* pSpaces
  pSymbol ":"
  t <- parseType
  pSymbol "."
  e <- parseExpr
  return $ Lam x t e

parseTLam :: Parser Expr
parseTLam = do
  pSymbol "/\\" <|> pSymbol "Λ"
  a <- parseIdent <* pSpaces
  pSymbol "."
  e <- parseExpr
  return $ TLam a e

parseTopExpr :: Parser (Maybe TopExpr)
parseTopExpr = pChoice "top"
  [ Just <$> pTry p
  , Just <$> Expr <$> parseExpr
  , Nothing <$ pSpaces ]
  where
    p = do
      x <- parseIdent <* pSpaces
      Bind x <$> (pSymbol "=" *> parseExpr)

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
parseTyVar = TyVar <$> parseIdent <* pSpaces

parseTyFun :: Type -> Parser Type
parseTyFun t = do
  pSymbol "->" <|> pSymbol "→"
  t' <- parseType
  return $ TyFun t t'

parseTyPoly :: Parser Type
parseTyPoly = do
  pSymbol "forall" <|> pSymbol "∀"
  a <- parseIdent <* pSpaces
  pSymbol "."
  t <- parseType
  return $ TyPoly a t
