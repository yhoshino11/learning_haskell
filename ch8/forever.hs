import Control.Monad
import Data.Char

main = forever $ do
  putStr "Give me some input: "
  line <- getLine
  putStrLn $ map toUpper line
