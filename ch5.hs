isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
{- applyTwice (3:) [1] -}

{- addTree' :: Int -> Int -> Int -> Int -}
{- addTree' x y z = x + y + z -}

addTree' :: Int -> Int -> Int -> Int
addTree' = \x -> \y -> \z -> x + y + z

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
{- zipWith' max [6, 3, 2, 1] [7, 3, 1, 5] -}

{- flip' :: (a -> b -> c) -> (b -> a -> c) -}
{- flip' f = g -}
  {- where g x y = f y x -}

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x
{- flip' zip [1..5] "hello" -}

{- flip' :: (a -> b -> c) -> b -> a -> c -}
{- flip' f = \x y -> f y x -}

{- map' :: (a -> b) -> [a] -> [b] -}
{- map' _ [] = [] -}
{- map' f (x:xs) = f x : map' f xs -}
{- map' (++ "!") ["BIFF", "BANG", "POW"] -}
{- map' fst [(1,2), (3,5)] -}
{- map fst [(1,2), (3,5)] -}

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

{- filter' :: (a -> Bool) -> [a] -> [a] -}
{- filter' _ [] = [] -}
{- filter' p (x:xs) -}
  {- | p x = x : filter' p xs -}
  {- | otherwise = filter' p xs -}
{- filter' odd [1..10] -}
{- filter' (< 15) (filter' even [1..20]) -}

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

{- numLongChains :: Int -}
{- numLongChains = length (filter isLong (map chain [1..100])) -}
  {- where isLong xs = length xs > 15 -}

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15)
                               (map chain [1..100]))
{- map (+3) [1..10] -}
{- map (\x -> x + 3) [1..10] -}
{- zipWith (\a b -> (a * 30 + 3) / b) (reverse [1..5]) [1..5] -}
{- map (\(a, b) -> a + b) [(1,2), (3,5), (6,3), (2,6), (2,5)] -}

{- sum' :: (Num a) => [a] -> a -}
{- sum' xs = foldl (\acc x -> acc + x) 0 xs -}
{- sum' [1..5] -}

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max
{- maximum' [1..10] -}

reverse' :: [a] -> [a]
{- reverse' = foldl (\acc x -> x : acc) [] -}
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

{- scanl (+) 0 [3,5,2,1] -}
{- scanr (+) 0 [3,5,2,1] -}

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1
{- sum (map sqrt [1..131]) -}
{- sum (map sqrt [1..130]) -}
{- sum $ map sqrt [1..130] -}

{- ($) :: (a -> b) -> a -> b -}
{- f $ x = f x -}
{- sqrt $ 3 + 4 + 9 -}

{- sum (filter (> 10) (map (*2) [2..10])) -}
{- sum $ filter (> 10) (map (*2) [2..10]) -}
{- sum $ filter (> 10) $ map (*2) [2..10] -}

{- (.) :: (b -> c) -> (a -> b) -> a -> c -}
{- f . g = \x -> f (g x) -}

{- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24] -}
{- map (negate . abs) [5,-3,-6,7,-3,2,-19,24] -}

{- map (\xs -> negate (sum (tail xs))) [[1..5], [3..6], [1..7]] -}
{- map (negate . sum . tail) [[1..5], [3..6], [1..7]] -}

{- sum (replicate 5 (max 6.7 8.9)) -}
{- (sum . replicate 5) (max 6.7 8.9) -}
{- sum . replicate 5 $ max 6.7 8.9 -}

{- replicate 2 (product (map (*3) (zipWith max [1,2] [4,5]))) -}
{- replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5] -}

{- Point Free Style -}
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs
sum' = foldl (+) 0

{- fn x = ceiling (negate (tan (cos (max 50 x)))) -}
{- fn = ceiling . negate . tan . cos . max 50 -}

oddSquareSum :: Integer
{- oddSquareSum = sum (takeWhile (< 10000) (filter odd (map (^2) [1..]))) -}
oddSquareSum = sum . takeWhile (< 10000) . filter odd $ map (^2) [1..]
