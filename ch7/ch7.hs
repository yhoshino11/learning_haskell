import qualified Data.Map as Map

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)


data Car = Car { company :: String
               , model :: String
               , year :: Int } deriving (Show)
{- let car = Car { company="TOYOTA", model="Lexus", year=2015 } -}

tellCar :: Car -> String
tellCar (Car { company = c, model = m, year = y })
  = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n)
  = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n)
  = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)


data UniquePerson = UniquePerson { u_firstName :: String
                                 , u_lastName :: String
                                 , u_age :: Int } deriving (Eq, Show, Read)


data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)


type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
phoneBook :: PhoneBook
phoneBook
  = [("betty",   "111-2222")
    ,("bonnie",  "222-3333")
    ,("patsy",   "333-4444")
    ,("lucille", "444-5555")
    ,("wendy",   "555-6666")
    ,("penny",   "666-7777")]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook


data LockerState = Taken | Free
                   deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
  Just (state, code) -> if state /= Taken
                        then Right code
                        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
  [(100, (Taken, "AB111"))
  ,(101, (Free, "BC222"))
  ,(102, (Free, "CD333"))
  ,(105, (Free, "DE444"))
  ,(107, (Taken, "EF555"))
  ,(109, (Taken, "FG666"))]


{- data List a = Empty | Cons a (List a) -}
              {- deriving (Show, Read, Eq, Ord) -}


infixr 5 ^++
(^++) :: [a] -> [a] -> [a]
[] ^++ ys = ys
(x:xs) ^++ ys = x:(xs ^++ ys)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x <  a = Node a (treeInsert x left) right
  | x >  a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x <  a = treeElem x left
  | x >  a = treeElem x right

{- let nums = [8,6,4,1,7,3,5] -}
{- let numsTree = foldr treeInsert EmptyTree nums -}
{- numsTree -}
{- 8 `treeElem` numsTree -}


{- class Eq a where -}
  {- (==) :: a -> a -> Bool -}
  {- (/=) :: a -> a -> Bool -}
  {- x == y = not (x /= y) -}
  {- x /= y = not (x == y) -}

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
  Red    == Red    = True
  Green  == Green  = True
  Yellow == Yellow = True
  _      == _      = False

instance Show TrafficLight where
  show Red    = "Red Light"
  show Yellow = "Yellow Light"
  show Green  = "Green Light"


class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where
  yesno = id

instance YesNo [Maybe a] where
  yesno (Just _) = True
  yesno Nothing  = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _         = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True

{- yesno $ length [] -}
{- yesno $ "haha" -}
{- yesno $ "" -}
{- yesno $ Just 0 -}
{- yesno $ Tree -}
{- yesno EmptyTree -}
{- yesno [] -}
{- yesno [0,0,0] -}

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult
  = if yesno yesnoVal
    then yesResult
    else noResult

{- yesnoIf [] "YEAH!" "NO!" -}


{- class Functor f where -}
  {- fmap :: (a -> b) -> f a -> f b -}

{- instance Functor [] where -}
  {- fmap = map -}

{- instance Functor Maybe where -}
  {- fmap f (Just x) = Just (f x) -}
  {- fmap f Nothing  = Nothing -}
