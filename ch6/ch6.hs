import Data.List
import Data.Char
import qualified Data.Map as Map

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub

{- words "hey these are the words in this sentence" -}

{- group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] -}
{- group ["boom", "bip", "bip", "boom", "boom"] -}

{- sort [5,4,3,7,2,1] -}
{- group $ sort ["boom", "bip", "bip", "boom", "boom"] -}

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group .sort . words
{- wordNums "hello world" -}

{- tails "party" -}
{- tails [1,2,3] -}

{- "hawaii" `isPrefixOf` "hawaii joe" -}
{- "haha"   `isPrefixOf` "ha" -}
{- "ha"     `isPrefixOf` "ha" -}

{- any (> 4) [1,2,3] -}
{- any (== 'F') "Frank Sobotka" -}
{- any (\x -> x > 5 && x < 10) [1,4,11] -}

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)
{- "art" `isIn` "party" -}
{- [1,2] `isIn` [1,3,5] -}
{- "art" `isInfixOf` "party" -}
{- [1,2] `isInfixOf` [1,3,5] -}

{- map ord "abcdefgh" -}

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg
{- encode 3 "hey mark" -}

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
{- decode 3 "kh|#pdun" -}

{- foldl (+) 0 (replicate 100 1) -}
{- foldl (+) 0 (replicate 1000000 1) -}
{- foldl' (+) 0 (replicate 1000000 1) -}

{- digitToInt '2' -}
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show
{- take 1 [x | x <- [1..], digitSum x == 40] -}

{- find (> 4) [3..7] -}
{- find odd [2,4,6,8,9] -}
{- find (== 'z') "mjolnir" -}

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

{- phoneBook = -}
  {- [("betty",  "555-2938"), -}
   {- ("bonnie", "452-2928"), -}
   {- ("patsy",  "493-2928"), -}
   {- ("lucille","205-2928"), -}
   {- ("wendy",  "939-8282"), -}
   {- ("penny",  "853-2492")] -}
{- findKey :: (Eq k) => k -> [(k, v)] -> v -}
{- findKey key xs = snd . filter (\(k, v) -> key == k) $ xs -}

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v):xs)
  | key == k = Just v
  | otherwise = findKey key xs
{- findKey "penny" phoneBook -}
{- findKey "me" phoneBook -}

{- Map.fromList [(3, "shoes"), (4, "trees"), (5, "bees")] -}
{- Map.fromList [("kima", "greggs"), ("jimmy", "mcnulty"), ("jay", "landsman")] -}
{- Map.fromList [("MS", 1), ("MS", 2), ("MS", 3)] -}

{- phoneBook :: Map.Map String String -}
{- phoneBook = Map.fromList $ -}
  {- [("betty",  "555-2938"), -}
   {- ("bonnie", "452-2928"), -}
   {- ("patsy",  "493-2928"), -}
   {- ("lucille","205-2928"), -}
   {- ("wendy",  "939-8282"), -}
   {- ("penny",  "853-2492")] -}
{- Map.lookup "betty" phoneBook -}
{- Map.lookup "wendy" phoneBook -}
{- Map.lookup "me" phoneBook -}
{- let newBook = Map.insert "grace" "341-9021" phoneBook -}
{- Map.lookup "grace" phoneBook -}
{- Map.size phoneBook -}
{- Map.size newBook -}

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit
{- string2digits "948-9282" -}

{- let intBook = Map.map string2digits phoneBook -}
{- Map.lookup "betty" intBook -}

phoneBook =
  [("betty",  "555-2938"),
   ("betty",  "342-2492"),
   ("bonnie", "452-2928"),
   ("patsy",  "493-2929"),
   ("patsy",  "493-2928"),
   ("lucille","205-2928"),
   ("wendy",  "939-8282"),
   ("penny",  "853-2492"),
   ("penny",  "555-2111")]
{- phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String -}
{- phoneBookToMap xs = Map.fromListWith add xs -}
  {- where add number1 number2 = number1 ++ ", " ++ number2 -}
{- Map.lookup "patsy" $ phoneBookToMap phoneBook -}
{- Map.lookup "wendy" $ phoneBookToMap phoneBook -}
{- Map.lookup "betty" $ phoneBookToMap phoneBook -}

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
{- Map.lookup "patsy" $ phoneBookToMap phoneBook -}

{- Map.fromListWith max [(2,3), (2,5), (2,100), (3,29), (3,22), (3,11), (4,22), (4,15)] -}
{- Map.fromListWith (+) [(2,3), (2,5), (2,100), (3,29), (3,22), (3,11), (4,22), (4,15)] -}
