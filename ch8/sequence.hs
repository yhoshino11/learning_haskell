{- main = do -}
  {- a <- getLine -}
  {- b <- getLine -}
  {- c <- getLine -}
  {- print [a,b,c] -}

main = do
  rs <- sequence [getLine, getLine, getLine]
  print rs
