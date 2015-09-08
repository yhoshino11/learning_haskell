import Control.Monad
import Data.Char

main = forever $ do
  line <- getLine
  putStrLn $ map toUpper line
