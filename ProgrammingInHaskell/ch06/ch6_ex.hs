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