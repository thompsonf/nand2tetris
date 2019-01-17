module Analyzer.Parser where

import Tokenizer.Types (Token)

data Parser a = Parser [Token] -> (a, [Token])

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = \input -> let (a, rest) = p ss 
                     in (f a, rest)

instance Functor Parser where
  fmap f (Parser p) = Parser (pmap f p)

instance Applicative Parser where  
  pure x = Parser \tokens -> (x, tokens)  
  (Parser f) <*> (Parser x) = Parser \tokens -> (f x, tokens)