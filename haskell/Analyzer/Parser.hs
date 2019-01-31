module Analyzer.Parser where

import Tokenizer.Types (Token)

data Parser a = Parser ([Token] -> (a, [Token]))

pmap :: (a -> b) -> Parser a -> Parser b
pmap f (Parser p) = Parser $ \input -> let (a, rest) = p input 
                                       in (f a, rest)

runParser :: Parser a -> [Token] -> (a, [Token])
runParser (Parser p) tokens = p tokens

instance Functor Parser where
  fmap = pmap

instance Applicative Parser where  
  pure x = Parser $ \tokens -> (x, tokens)  
  pf <*> px = Parser $ \tokens -> let (f, rest) = runParser pf tokens
    in runParser (f <$> px) rest