import System.IO
import Data.Char

main = do
  contents <- readFile "baabaa.txt"
  writeFile "baabaacaps.txt" (map toUpper contents)
