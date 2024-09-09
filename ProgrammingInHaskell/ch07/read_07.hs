sum :: Num a => [a] -> a
sum [] = 0
sum xs = foldr (+) 0 xs

product :: Num a => [a] -> a
product [] = 1
product xs = foldr (*) 1 xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f acc []     = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc []     = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)
