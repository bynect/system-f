{-# LANGUAGE CPP #-}
module Parse
  (
    parseExpr,
    parseTopExpr,
    parseType) where

import Control.Monad
import Expr
import Comb

parseIdent :: Parser String
parseIdent = do
  s  <- pMany1 $ pPred "identifier" pred
  s' <- pMany $ pPred "identifier" (== '\'')
  return $ s ++ s'
  where
      pred c = (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || c == '_'

-- Identifiers except expression-related keywords
parseIdentExpr :: Parser String
#ifdef SUGAR_LET
parseIdentExpr = do
  s <- parseIdent
  if s == "let" || s == "in"
    then pError "identifier" "keyword"
    else return s
#else
parseIdentExpr = parseIdent
#endif

-- Identifiers except type-related keywords
parseIdentType :: Parser String
parseIdentType = do
  s <- parseIdent
  if s == "forall"
    then pError "identifier" "keyword"
    else return s

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
#ifdef SUGAR_LET
      , parseLet
#endif
      , parseVar <* parseSpace ]

    p' e = do
        e' <- parseParen parseExpr <|> pTry parseVar
        parseSpace
        p' $ App e e'
      <|> do
        t <- parseSym "[" *> parseType <* parseSym "]"
        p' $ TApp e t
      <|> return e

parseVar :: Parser Expr
parseVar = Var <$> parseIdentExpr

parseLam :: Parser Expr
parseLam = do
  parseSym "\\" <|> parseSym "λ"
  x <- parseIdentExpr <* parseSpace
  parseSym ":"
  t <- parseType
  parseSym "."
  e <- parseExpr
  return $ Lam x t e

parseTLam :: Parser Expr
parseTLam = do
  parseSym "/\\" <|> parseSym "Λ"
  a <- parseIdentType <* parseSpace
  parseSym "."
  e <- parseExpr
  return $ TLam a e

#ifdef SUGAR_LET
-- NOTE: Let expressions can be transparently desugared to abstractions
--
-- let x:τ = e in e'
-- (λx:τ. e') e
parseLet :: Parser Expr
parseLet = do
  parseSym "let"
  x  <- parseIdentExpr <* parseSpace
  parseSym ":"
  t  <- parseType
  parseSym "="
  e  <- parseExpr
  parseSym "in"
  e' <- parseExpr
  return $ App (Lam x t e') e
#endif

parseTopExpr :: Parser (Maybe TopExpr)
parseTopExpr = parseSpace *> pChoice "top"
  [ Just    <$> pTry p
  , Just    <$> Expr <$> parseExpr
  , Nothing <$ pEof ]
  where
    p = do
      x <- parseIdentExpr <* parseSpace
      parseSym "="
      Bind x <$> parseExpr

parseType :: Parser Type
parseType = do
  t <- pBetween parseSpace parseSpace p
  pTry $ parseTyFun t <|> return t
  where
    p = pChoice "type"
      [ parseParen parseType
      , parseTyPoly
      , TyVar <$> parseIdentType ]

parseTyFun :: Type -> Parser Type
parseTyFun t = do
  parseSym "->" <|> parseSym "→"
  t' <- parseType
  return $ TyFun t t'

parseTyPoly :: Parser Type
parseTyPoly = do
  parseSym "forall" <|> parseSym "∀"
  a <- parseIdentType <* parseSpace
  parseSym "."
  t <- parseType
  return $ TyPoly a t
