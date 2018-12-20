import VM.Parse (parseFile)
import VM.ToASM (bootstrap, genCode)
import VM.Types
import System.Directory (isDirectory, listDirectory)
import System.Environment
import System.FilePath (takeBaseName, takeExtension)

main = do
  [input, output] <- getArgs
  isDir <- isDirectory input
  files <- if isDir
  	then listDirectory input >>= filter (\f -> takeExtension f == ".vm")
  	else return input
  asms <- sequence $ map genCodeForFile files
  let withBootstrap = if length asms > 1 then bootstrap:asms else asms
  writeFile output $ unlines withBootstrap

genCodeForFile :: String -> IO [String]
genCodeForFile file = readFile file >>= \source -> genCode file source