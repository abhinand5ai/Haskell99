import Control.Arrow (Arrow (first))

factorial :: Int -> Int
factorial n
  | n < 0 = error "factorial: negative argument"
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

(^) :: (Num a, Integral b) => a -> b -> a
x ^ 0 = 1
x ^ n = x * (x Main.^ (n - 1))

euclid :: Int -> Int -> Int
euclid a b = case (a, b) of
  (a, b)
    | a == b -> a
    | a > b -> euclid (a - b) b
    | a < b -> euclid a (b - a)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort first) (msort second)
  where
    (first, second) = splitAt (length xs `div` 2) xs

-- 6
and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = if x then and' xs else False

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = case x of
  [] -> concat' xs
  l : ls -> l : concat' (ls : xs)

replicate' :: Int -> a -> [a]

replcate' 0 _ = []

replicate' n x = x : replicate' (n - 1) x

nth :: [a] -> Int -> a
nth (x : xs) 0 = x
nth (x : xs) n = nth xs (n - 1)
nth _ _ = error "invalid index"

elem' :: (Eq a) => [a] -> a -> Bool
elem' [] _ = False
elem' (l : ls) x = (x == l) || elem' ls x

-- 9
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + (sum' xs)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n [] = []
take' n (x : xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' [] = error "empty list"
last' (x : xs) = last' xs
last' [x] = x