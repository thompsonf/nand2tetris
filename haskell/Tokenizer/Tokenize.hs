module Tokenizer.Tokenize 
( tokenize
) where

import Tokenizer.Types

tokenize :: String -> [Token]
tokenize _ = []

stripLineComment :: String -> String
stripLineComment ('/':'/':xs) = dropWhile (/= '\n') xs
stripLineComment xs = xs

stripBlockComment :: String -> String
stripBlockComment ('/':'*':xs) = stripBlockCommentHelper xs
stripBlockComment xs = xs

stripBlockCommentHelper :: String -> String
stripBlockCommentHelper ('*':'/':xs) = xs
stripBlockCommentHelper (x:xs) = stripBlockCommentHelper xs