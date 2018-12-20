import VM.Parse (parseFile)
import VM.ToASM (bootstrap, genCode)
import VM.Types
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment
import System.FilePath (takeBaseName, takeExtension, (</>))

main = do
  [input, output] <- getArgs
  isDir <- doesDirectoryExist input
  files <- if isDir
    then getSourceFiles input
    else return [input]
  asms <- sequence $ map genCodeForFile files
  let withBootstrap = if length asms > 1 then (bootstrap:asms) else asms
  writeFile output $ unlines (concat withBootstrap)

getSourceFiles :: String -> IO [String]
getSourceFiles dir = do
  allFiles <- listDirectory dir
  let vms = filter (\f -> takeExtension f == ".vm") allFiles
  return $ map (\f -> dir </> f) vms

genCodeForFile :: String -> IO [String]
genCodeForFile file = do
  source <- readFile file
  return $ genCode (takeBaseName file) (parseFile source)