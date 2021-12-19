{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Comb
  (
    Error(..),
    Parser,
    pAny, pEof,
    pError,
    pTry,
    (<|>),
    pPred,
    pMany, pMany',
    pSepBy, pSepBy',
    pChoice, pManyTill,
    pChar,
    pSpace, pSpaces,
    pString,
    pBetween,
    pRun
  ) where

import Control.Monad
import Control.Applicative (liftA2)
import Data.Functor
import Data.Char

data Error = Error { expected :: String
                   , found    :: String }

instance Show Error where
  show (Error expected found) = "Expected " ++ expected ++ " instead of " ++ found

newtype Parser a = Parser { runParser :: String -> (String, Either Error a) }
                   deriving Functor

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
  []     -> ("", Left $ Error "any" "eof")

pEof :: Parser ()
pEof = Parser $ \case
  [] -> ("", Right ())
  xs -> (xs, Left $ Error "eof" xs)

pError :: String -> String -> Parser a
pError expected found = Parser $ \s -> (s, Left $ Error expected found)

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

pMany, pMany' :: Parser a -> Parser [a]
pMany  p = pMany' p <|> pure []
pMany' p = liftA2 (:) p $ pMany p

pSepBy, pSepBy' :: Parser a -> Parser b -> Parser [a]
pSepBy  p p' = pSepBy' p p' <|> pure []
pSepBy' p p' = liftA2 (:) p $ pMany (p' >> p)

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

pRun :: Parser a -> String -> Either Error a
pRun p s = snd $ runParser (p <* pEof) s
