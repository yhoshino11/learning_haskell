import Control.Monad

main = do
  input <- getLine
  when (input == "haskell") $ do
    putStrLn input
