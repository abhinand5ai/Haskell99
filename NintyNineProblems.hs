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
isPalindrome (x : xs) = x == last xs && isPalindrome (init xs)

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
decodeModified :: [Item a] -> [a]
decodeModified =
  let decodeItem :: Item a -> [a]
      decodeItem x = case x of
        Multiple n a -> replicate n a
        Single a -> [a]
   in concatMap decodeItem

-- #13
encodeDirect :: (Eq a) => [a] -> [Item a]
encodeDirect [] = []
encodeDirect [a] = [Single a]
encodeDirect (x : xs) =
  let getItem :: Item a -> a
      getItem (Single a) = a
      getItem (Multiple _ a) = a

      getItemCount :: Item a -> Int
      getItemCount (Single _) = 1
      getItemCount (Multiple n _) = n
      encodedTail = encodeDirect xs
      nextElement = head encodedTail
      nextElementCount = getItemCount nextElement
   in if getItem nextElement == x
        then Multiple (nextElementCount + 1) x : tail encodedTail
        else Single x : encodedTail

-- # 14
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- # 15
repli :: [a] -> Int -> [a]
repli x n = concatMap (replicate n) x

-- # 16

dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs n = take (n - 1) xs ++ dropEvery (drop n xs) n

-- # 17
split :: [a] -> Int -> ([a], [a])
split xs n = splitAt n xs

-- # 18
slice :: [a] -> Int -> Int -> [a]
slice xs start end = drop (start - 1) $ take end xs

-- # 19
rotate :: [a] -> Int -> [a]
rotate xs k =
  let len = length xs
      k_final = if k >= 0 then rem k len else len - rem (-1 * k) len
   in drop k_final xs ++ reverse (take k_final xs)

-- # 20
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = (xs !! k, take (k - 1) xs ++ drop k xs)

-- # 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x ls 1 = x : ls
insertAt x ls i = head ls : insertAt x (tail ls) (i - 1)

-- # 22
range :: Int -> Int -> [Int]
range x y = if x == y then [x] else x : range (x  + 1) y

-- # 23

--------------------------------------
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
