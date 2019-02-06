import Analyzer.Analyze (analyze)
import Compiler.Compiler (compile)
import Tokenizer.Tokenize (tokenize)

import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment
import System.FilePath (takeExtension, (</>))

main = do
  [inputoutput <- getArgs
  isDir <- doesDirectoryExist input
  files <- if isDir
    then getSourceFiles input
    else return [input]
  sequence $ map genCodeForFile files

getSourceFiles :: String -> IO [String]
getSourceFiles dir = do
  allFiles <- listDirectory dir
  let vms = filter (\f -> takeExtension f == ".jack") allFiles
  return $ map (\f -> dir </> f) vms

genCodeForFile :: String -> IO ()
genCodeForFile file = do
  let output = replaceExtension file ".vm"
  source <- readFile file
  let commands = compile . analyze . tokenize source
  -- writeFile output $ unlines (map prettyCommand commands)
  return ()