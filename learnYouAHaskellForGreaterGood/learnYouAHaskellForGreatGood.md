# Higher Order Functions

## Currying
```
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x
```

## Sections

```
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
```

```
isUpperAlphanum :: Char -> Boolean
isUpperAlphanum = (`elem` ['A'..'Z'])
```

## Lambda Functions
```
add1 = map (\x -> x + 1) [1..10]
mult3 = map (*3) [1..10]

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

```
## Fold

### foldl
```

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\ acc x -> acc + x ) 0 [1..100]
sum1 = foldl (+) 0

```
Left Fold is equivalent to   ```foldl g [3,4,5,6] ~ g (g (g (g z 3) 4) 5) 6```

### foldr
```
map' :: (a)
map' f = foldr (\ x acc -> f x : acc ) []

reverse':: [a] -> [a]
reverse' = foldl (\ acc x -> x:acc) []
reverse' = foldl (flip (:)) []
reverse' = foldr (\ x acc -> acc ++ [x]) []


filter' :: (a -> bool) -> [a] -> [a]
filter' f xs = foldl (\ x acc -> if f x then x:acc else acc) [] xs

last' :: [a] -> a
last = foldll (\ _ x -> x)

elem' :: (Eq a) => [a] -> a
elem' ys y = foldr (\ x acc -> if x == y then True else acc) False ys
```

Right fold is equivalent to     ```foldr f [3,4,5,6] ~ f 3 (f 4 (f 5 (f 6 z)))```

Right fold allows to work in infinite lists sometimes
```
and` :: [Bool] -> [Bool]
and` xs = foldr (&&) True xs
```

## Scans
#### scanl
```
scanl (+) 0 [3,5,2,1]
```

## function application with $
```
($) :: (a -> b) -> a -> b
f $  = f 

sm = sum (filter (> 10) (map (*2) [2..10]))
sm1 = sum $ filter (> 10) $ map (*2) [2..10] 

mp = map ($ 3) [(4+), (10*), (^2), sqrt]

```

## function composition
```
(.) ::  (b -> c) -> (a -> b) -> a -> c
f . g = \ x -> f (g x)

map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
```

with multiple params use partial functions
```
replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]

oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]


```

# Modules

```
import Data.List (nub, sort)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

```
Importing to GHCi

```
ghci> :m + Data.List Data.Map Data.Set
```

Hidging a method

```
import Data.List  hiding (nub)
```

Handle method name clashes

```
import qualified Data.Map as M

M.filter (<10) [2..10]
```

count freq of words

```
wordNums :: String -> [(String, Int)]
wordNums = map (\ xs -> (head xs, length xs)) . group . words
```
