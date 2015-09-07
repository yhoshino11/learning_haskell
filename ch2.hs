removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addTree :: Int -> Int -> Int -> Int
addTree x y z = x + y + z

factrial :: Integer -> Integer
factrial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r
