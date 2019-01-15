import System.Environment
import Tokenizer.Tokenize
import Tokenizer.ToXML

main = do
  [input, output] <- getArgs
  source <- readFile input
  writeFile output (toXML (tokenize source))