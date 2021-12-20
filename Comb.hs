{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Comb
  (
    ParseError,
    Parser,
    pAny,
    pEof,
    pError,
    pTry,
    (<|>),
    pPred,
    pMany, pMany1,
    pSepBy, pSepBy1,
    pChoice, pManyTill,
    pChar,
    pSpace, pSpaces,
    pString,
    pBetween,
    pEol,
    pRun
  ) where

import Control.Monad
import Control.Applicative (liftA2)
import Data.Functor
import Data.Char

data ParseError = PError { expected :: String
                          , found    :: String }

instance Show ParseError where
  show (PError expected found) = "Expected " ++ expected ++ " instead of " ++ found

newtype Parser a = Parser { runParser :: String -> (String, Either ParseError a)
                          } deriving Functor

instance Applicative Parser where
  pure = return
  p <*> p' = Parser $ \s -> case runParser p s of
    (s', Right f) -> fmap f <$> runParser p' s'
    (s', Left e)  -> (s', Left e)


instance Monad Parser where
  return a = Parser $ \s -> (s, Right a)
  p >>= p' = Parser $ \s -> case runParser p s of
    (s', Right a) -> runParser (p' a) s'
    (s', Left e)  -> (s', Left e)

pAny :: Parser Char
pAny = Parser $ \case
  (x:xs) -> (xs, Right x)
  []     -> ("", Left $ PError "any" "eof")

pEof :: Parser ()
pEof = Parser $ \case
  [] -> ("", Right ())
  xs -> (xs, Left $ PError "eof" xs)

pError :: String -> String -> Parser a
pError expected found = Parser $ \s -> (s, Left $ PError expected found)

pTry :: Parser a -> Parser a
pTry p = Parser $ \s -> case runParser p s of
  (s', Right a) -> (s', Right a)
  (_, e)        -> (s, e)

(<|>) :: Parser a -> Parser a -> Parser a
p <|> p' = Parser $ \s -> case runParser p s of
  (s', Right a) -> (s', Right a)
  (s', Left e) | s == s'   -> runParser p' s
               | otherwise -> (s', Left e)

pPred :: String -> (Char -> Bool) -> Parser Char
pPred expected pred = pTry $ do
  c <- pAny
  case pred c of
    True  -> pure c
    False -> pError expected [c]

pMany, pMany1 :: Parser a -> Parser [a]
pMany  p = pMany1 p <|> pure []
pMany1 p = liftA2 (:) p $ pMany p

pSepBy, pSepBy1 :: Parser a -> Parser b -> Parser [a]
pSepBy  p p' = pSepBy1 p p' <|> pure []
pSepBy1 p p' = liftA2 (:) p $ pMany (p' >> p)

pChoice :: String -> [Parser a] -> Parser a
pChoice expected = foldr (<|>) (pError expected "other")

pManyTill :: Parser a -> Parser b -> Parser [a]
pManyTill p p' = go
  where
    go = do
        _ <- p'
        return []
      <|> do
        x <- p
        xs <- go
        return (x:xs)

pChar :: Char -> Parser Char
pChar c = pPred [c] (== c)

pSpace :: Parser Char
pSpace = pPred "space" isSpace

pSpaces :: Parser String
pSpaces = pMany pSpace

pString :: String -> Parser String
pString = traverse pChar

pBetween :: Parser a -> Parser b -> Parser c -> Parser c
pBetween b a p = b *> p <* a

pEol :: Parser ()
pEol = (void $ pChar '\n') <|> pEof

pRun :: Parser a -> String -> Either ParseError a
pRun p s = snd $ runParser (p <* pEof) s
