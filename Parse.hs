module Parse
  (
    parseExpr,
    parseTopExpr,
    parseType) where

import Expr
import Comb

parseIdent :: Parser String
parseIdent = do
  s <- pMany1 $ pPred "identifier" pred
  s' <- pMany $ pPred "identifier" (== '\'')
  return $ s ++ s'
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
    -- NOTE: Assumes eof and newline are the same
    lineComment  = pTry (pString "--") *> pManyTill pAny pEof
    blockComment = pTry (pString "{-") *> pManyTill pAny (pTry $ pString "-}")

parseExpr :: Parser Expr
parseExpr = p >>= p'
  where
    p = pChoice "expression"
      [ parseParen parseExpr
      , parseLam
      , parseTLam
      , parseVar <* parseSpace ]

    p' e = do
        e' <- parseParen parseExpr <|> parseVar
        parseSpace
        p' $ App e e'
      <|> do
        t <- parseSym "[" *> parseType <* parseSym "]"
        p' $ TApp e t
      <|> return e

parseVar :: Parser Expr
parseVar = Var <$> parseIdent

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
