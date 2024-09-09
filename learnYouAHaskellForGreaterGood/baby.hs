import           Data.Char    (chr, digitToInt, ord)
import           Data.List    (find, group, sort)
import qualified Data.Map     as Map
import           Record
import           Shapes
import           TrafficLight
import           Tree

doubleMe x = x + x

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

lostNumber = [4, 8, 15, 16, 23, 42]

toInfinityAndBeyond = [1 ..]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY YOU!!"
lucky _ = "NOT LUCKY"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

head' :: [a] -> a
head' []    = error "No head for empty lists!"
head' (x:_) = x

reqList :: (Eq a, Num a) => [a] -> [Char]
reqList l =
  case l of
    4:_ -> "this is what I want"
    _   -> "Not Quite what I want"

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs > 5

listOfFuncs = map (*) [1 ..]

lambdaPatternMatch =
  map (uncurry (+)) [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 7), (7, 8)]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- addThree' :: Int -> Int -> Int -> Int
-- adddThree' = \x -> \y -> \z -> x + y + z
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\acc x -> f x : acc) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldr (\y acc -> y == x || acc) False

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- reverse' :: [a] -> [a]
-- reverse' [] = []
-- reverse' (x:xs) = reverse' xs ++ [x]
last' :: [a] -> a
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' = foldr (&&) True

scn = scanl (+) 0 [3, 5, 2, 1]

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
composition :: [Integer]
composition = map (negate . sum . tail) [[1 .. 5], [3 .. 6], [1 .. 7]]

wordNums :: String -> [(String, Int)]
wordNums = map (\xs -> (head xs, length xs)) . group . sort . words

encode :: Int -> String -> String
encode offset = map (chr . (+ offset) . ord)

decode :: Int -> String -> String
decode shift = encode (negate shift)

sumDigits :: Int -> Int
sumDigits = sum . map digitToInt . show

firstWithDigitSum :: Int -> Maybe Int
firstWithDigitSum sum = find (\x -> sumDigits x == sum) [1 ..]

phoneBook =
  [ ("betty", "555-2938")
  , ("bonnie", "452-2928")
  , ("patsy", "493-2928")
  , ("lucille", "205-2928")
  , ("wendy", "939-8282")
  , ("penny", "853-2492")
  ]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key = snd . head . filter (\(k, _) -> key == k)

-- Can throw exceptions if not found
findKey2 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey2 _ [] = Nothing
findKey2 key ((k, v):xs)
  | key == k = Just v
  | otherwise = findKey2 key xs

findKey3 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey3 key =
  foldl
    (\acc (k, v) ->
       if k == key
         then Just v
         else acc)
    Nothing

phoneBookMap = Map.fromList phoneBook

phoneBookDup =
  [ ("betty", "555-2938")
  , ("betty", "342-2492")
  , ("bonnie", "452-2928")
  , ("patsy", "493-2928")
  , ("patsy", "943-2929")
  , ("patsy", "827-9162")
  , ("lucille", "205-2928")
  , ("wendy", "939-8282")
  , ("penny", "853-2492")
  , ("penny", "555-2111")
  ]

phoneBookToMap :: (Ord k) => [(k, v)] -> Map.Map k [v]
phoneBookToMap = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))

--- Making Own Data Types and Type Classes
circles = map (Circle $ Point 1 2) [3, 4, 5, 6]

data Vector a =
  Vector a a a
  deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i * l + j * m + k * n

vmult :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vmult` (Vector l m n) = Vector (i * l) (j * m) (k * n)

mysteryDude =
  "Person { firstName =\"Michael\""
    ++ ", lastName =\"Diamond\""
    ++ ", age = 43}"

magicMike = read mysteryDude :: Person

mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}

adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}

mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

days = [Thursday .. Saturday]

days2 = [minBound .. maxBound] :: [Day]

data LockerState
  = Taken
  | Free
  deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker" ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
      if state == Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]

infixr 5 :-:

data List a
  = Empty
  | a :-: (List a)
  deriving (Show, Read, Eq, Ord)

infixr 5 ^++

(^++) :: List a -> List a -> List a
Empty ^++ ys      = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

nums = [8, 6, 4, 1, 7, 3, 5]

numsTree = foldr treeInsert EmptyTree nums

val sameLight = Red ~~ Red

-- Functors
appendJust = fmap (++ " Sai") (Just "Abhinand")

fmapTree = fmap (* 4) (foldr treeInsert EmptyTree [5, 7, 3])
