import Data.List (find)
import Main (positions)

sumSquared :: Int -> Int
sumSquared n = sum [x ^ 2 | x <- [1 .. n]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [1 .. m], y <- [1 .. n]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1 .. n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], x == sum (init (factors x))]

ls = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

ls' = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

positions' :: (Eq a) => a -> [a] -> [Int]
positions' = find

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]