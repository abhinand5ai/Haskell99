import           Control.Arrow (Arrow (first))
import           Data.Char     (chr, isAsciiLower, ord)

ls = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [a | (a, _) <- ps]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Int -> Bool
prime x = factors x == [1, x]

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: (Ord a) => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (i, v) <- zip [0 ..] xs, v == x]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n ch = int2let ((let2int ch + n) `mod` 26)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = fromIntegral n / fromIntegral m * 100

lower :: String -> Int
lower xs = length [x | x <- xs, isAsciiLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lower xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table =
  [ 8.1
  , 1.5
  , 2.8
  , 4.2
  , 12.7
  , 2.2
  , 2.0
  , 6.1
  , 7.0
  , 0.2
  , 0.8
  , 4.0
  , 2.4
  , 6.7
  , 7.5
  , 1.9
  , 0.1
  , 6.0
  , 6.3
  , 9.0
  , 2.8
  , 1.0
  , 2.4
  , 0.2
  , 2.0
  , 0.1
  ]

crack :: String -> String
crack xs = encode (-factor) xs
  where
    scores = [chisqr table (rotate n (freqs xs)) | n <- [0 .. 25]]
    factor = head (positions (minimum scores) scores)
