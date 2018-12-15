import VM.Parse (parseFile)
import VM.ToASM (toASM)
import VM.Types
import System.Environment
import System.FilePath (takeBaseName)

main = do
  [input, output] <- getArgs
  source <- readFile input
  let ast = parseFile source
  writeFile output $ unlines $ genCode input ast

genCode :: String -> [Command] -> [String]
genCode fpath commands = genCodeHelper fname (zip [0..] commands)
  where fname = takeBaseName fpath

genCodeHelper :: String -> [(Int, Command)] -> [String]
genCodeHelper _ [] = []
genCodeHelper fname ((lineNum, command):xs) = ("// " ++ show command):(current ++ rest)
  where
    current = toASM fname lineNum command
    rest = genCodeHelper fname xs