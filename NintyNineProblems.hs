module NintyNineProblems
  ( myLast,
    elementAt,
    myButLast,
  )
where

import Data.Function (fix)

-- #1
myLast :: [a] -> a
myLast [] = error "No End for empty lists"
myLast [x] = x
myLast (x : xs) = myLast xs

-- #2
myButLast :: [a] -> a
myButLast [] = error "No But last for empty"
myButLast [x] = error "No But Last for single element list"
myButLast (a : [_]) = a
myButLast (_ : xs) = myButLast xs

myButLast' = last . init

-- #3
elementAt :: [a] -> Int -> a
elementAt ls k = ls !! k

-- #4
myLen :: [a] -> Int
myLen [] = 0
myLen (_ : xs) = 1 + myLen xs

-- #5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- #6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x : xs) = (x == last xs) && isPalindrome (init xs)

-- #7
data NestedList a = Elem a | List [NestedList a]

nList = List [Elem 2, List [List [Elem 2]]]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List xs) = concatMap myFlatten xs

-- #8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x : xs) = if x == head xs then compress (x : tail xs) else x : compress xs

-- #9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x : xs) =
  let packed = pack xs
   in if x == head (head packed) then (x : head packed) : tail packed else [x] : packed

-- #10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\xs -> (myLen xs, head xs)) (pack xs)

-- #11
data Item x = Multiple Int x | Single x deriving (Show)

encodeModified :: (Eq a) => [a] -> [Item a]
encodeModified =
  let tupleToItem (1, x) = Single x
      tupleToItem (n, x) = Multiple n x
   in map tupleToItem . encode

-- #12

nCr :: Int -> Int -> Int
nCr _ 0 = 1
nCr n r
  | n >= r = nCr (n - 1) r + nCr (n - 1) (r - 1)
  | otherwise = 0

fib :: (Int -> Integer) -> Int -> Integer
fib f 0 = 0
fib f 1 = 1
fib f n = f (n - 1) + f (n - 2)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

v2 = fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1)) 5

v1 = let rec n = if n <= 1 then 1 else n * rec (n - 1) in rec 5

fibMemo :: Int -> Integer
fibMemo = fix (memoize . fib)
