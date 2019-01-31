import System.Environment
import Analyzer.Analyze
import Analyzer.ToXML
import Tokenizer.Tokenize

main = do
  [input, output] <- getArgs
  source <- readFile input
  writeFile output $ (toXML . analyze . tokenize) source