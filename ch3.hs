lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factrial :: Int -> Int
factrial 0 = 1
factrial n = n * factrial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

xs = [(1, 3), (4, 3), (2, 4)]
{- [a + b | (a, b) <- xs] -}

{- head' :: [a] -> a -}
{- head' []    = error "Can't call head on an empty list." -}
{- head' (x:_) = x -}

head' :: [a] -> a
head' xs = case xs of []    -> error "Can't call head on an empty list."
                      (x:_) -> x

tell :: (Show a) => [a] -> String
tell []       = "The list is empty"
tell (x:[])   = "The list is one element: "  ++ show x
tell (x:y:[]) = "The list is two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_)  = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal."
  | weight / height ^ 2 <= 30.0 = "You're overweight!"
  | otherwise                   = "You're a whale, congratulations!"

{- bmiTell' :: Double -> Double -> String -}
{- bmiTell' weight height -}
  {- | bmi <= 18.5 = "You're underweight!" -}
  {- | bmi <= 25.0 = "You're supposedly normal." -}
  {- | bmi <= 30.0 = "You're overweight!" -}
  {- | otherwise   = "You're a whale, congratulations!" -}
  {- where bmi = weight / height ^ 2 -}

{- bmiTell' :: Double -> Double -> String -}
{- bmiTell' weight height -}
  {- | bmi <= underweight = "You're underweight!" -}
  {- | bmi <= normal      = "You're supposedly normal." -}
  {- | bmi <= overweight  = "You're overweight!" -}
  {- | otherwise          = "You're a whale, congratulations!" -}
  {- where bmi         = weight / height ^ 2 -}
        {- underweight = 18.5 -}
        {- normal      = 25.0 -}
        {- overweight  = 30.0 -}

bmiTell' :: Double -> Double -> String
bmiTell' weight height
  | bmi <= underweight = "You're underweight!"
  | bmi <= normal      = "You're supposedly normal."
  | bmi <= overweight  = "You're overweight!"
  | otherwise          = "You're a whale, congratulations!"
  where bmi         = weight / height ^ 2
        (underweight, normal, overweight) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b     = b
  | otherwise  = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b    = EQ
  | a <= b    = LT
  | otherwise = GT

badGreeting :: String
badGreeting = "Who are you?"

niceGreeting :: String
niceGreeting = "Hello!"

greet :: String -> String
greet "Juan"     = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name       = badGreeting  ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

{- calcBmis' :: [(Double, Double)] -> [Double] -}
{- calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2] -}

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^ 2
  in sideArea * topArea

{- 4 * (let a = 9 in a + 1) + 2 -}
{- 42 -}

{- [let square x = x * x in (square 5, square 3, square 2)] -}
{- [(25,9,4)] -}

{- (let a = 100; b = 200; c = 300 in a * b * c, -}
 {- let foo = "Hey "; bar = "there!" in foo ++ bar) -}
{- (6000000,"Hey there!") -}

{- (let (a, b, c) = (1, 2, 3) in a + b + c) * 100 -}
{- 600 -}

describeList :: [a] -> String
describeList ls = "The list is "
                  ++ case ls of []  -> "empty."
                                [x] -> "a singleton list."
                                xs  -> "a longer list."

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
  where what []  = "empty."
        what [x] = "a singleton list."
        what xs  = "a longer list."
