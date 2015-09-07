doubleMe x = x + x

{- doubleUs x y = x * 2 + y * 2 -}
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

{- zip [1..] (replicate 4 4) -}

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

rightTriangles = [(a, b, c)| c <- [1..10], a <- [1..c], b <- [1..a], a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 24]
