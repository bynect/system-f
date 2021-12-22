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
    pOpt,
    pPred,
    pChoice, pManyTill,
    pMany, pMany1,
    pChar,
    pSpace, pSpaces,
    pString,
    pBetween,
    pEol,
    pRun
  ) where

import Control.Applicative (liftA2)
import Control.Monad
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
  []     -> ("", Right ())
  (x:xs) -> (x:xs, Left $ PError "eof" [x])

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

pOpt :: a -> Parser a -> Parser a
pOpt x p = p <|> return x

pPred :: String -> (Char -> Bool) -> Parser Char
pPred expected pred = pTry $ do
  c <- pAny
  if pred c
     then pure c
     else pError expected [c]

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

pMany, pMany1 :: Parser a -> Parser [a]
pMany  p = pMany1 p <|> pure []
pMany1 p = liftA2 (:) p $ pMany p

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
