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

parseSym :: String -> Parser String
parseSym s = pString s <* parseSpace

parseParen :: Parser a -> Parser a
parseParen p = pBetween (parseSym "(") (parseSym ")") p

parseSpace :: Parser ()
parseSpace = pChoice "whitespace"
  [ lineComment  *> parseSpace
  , blockComment *> parseSpace
  , pSpace       *> parseSpace
  , return () ]
  where
    lineComment  = pTry (pString "--") *> pManyTill pAny pEof
    blockComment = pTry (pString "{-") *> pManyTill pAny (pTry $ pString "-}")

parseExpr :: Parser Expr
parseExpr = do
  e <- pBetween parseSpace parseSpace p
  es <- pMany $ p' <* parseSpace
  return $ foldl App e es
  where
    p = pChoice "expression"
      [ parseParen parseExpr
      , parseLam
      , parseTLam
      , parseType'
      , Var <$> parseIdent ]

    p' = pChoice "argument"
      [ parseParen parseExpr
      , parseType'
      , Var <$> parseIdent ]

parseType' :: Parser Expr
parseType' = do
  parseSym "["
  t <- parseType
  parseSym "]"
  return $ Type t

parseLam :: Parser Expr
parseLam = do
  parseSym "\\" <|> parseSym "λ"
  x <- parseIdent <* parseSpace
  parseSym ":"
  t <- parseType
  parseSym "."
  e <- parseExpr
  return $ Lam x t e

parseTLam :: Parser Expr
parseTLam = do
  parseSym "/\\" <|> parseSym "Λ"
  a <- parseIdent <* parseSpace
  parseSym "."
  e <- parseExpr
  return $ TLam a e

parseTopExpr :: Parser (Maybe TopExpr)
parseTopExpr = parseSpace *> pChoice "top"
  [ Just    <$> pTry p
  , Just    <$> Expr <$> parseExpr
  , Nothing <$ pEof ]
  where
    p = do
      x <- parseIdent <* parseSpace
      Bind x <$> (parseSym "=" *> parseExpr)

parseType :: Parser Type
parseType = do
  t <- pBetween parseSpace parseSpace p
  pTry $ parseTyFun t <|> return t
  where
    p = pChoice "type"
      [ parseParen parseType
      , parseTyPoly
      , TyVar <$> parseIdent ]

parseTyFun :: Type -> Parser Type
parseTyFun t = do
  parseSym "->" <|> parseSym "→"
  t' <- parseType
  return $ TyFun t t'

parseTyPoly :: Parser Type
parseTyPoly = do
  parseSym "forall" <|> parseSym "∀"
  a <- parseIdent <* parseSpace
  parseSym "."
  t <- parseType
  return $ TyPoly a t
