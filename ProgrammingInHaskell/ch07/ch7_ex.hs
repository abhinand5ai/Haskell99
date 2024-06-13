import Data.Binary (decode)
import Data.Char (chr, ord)
import Data.List (sort)
import Data.Text.Lazy.Builder.Int (decimal)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

-- f x1 (f x2 (f x3 v))

foldl' :: (a -> b -> b) -> b -> [a] -> b
foldl' f v [] = v
foldl' f v (x : xs) = foldl' f (f x v) xs

type Bit = Int

bin2Int :: [Bit] -> Int
bin2Int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

b2I :: [Bit] -> Int
b2I = foldr (\x y -> x + 2 * y) 0

int2Bin :: Int -> [Bit]
int2Bin x = mod x 2 : int2Bin (div x 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (make8 . int2Bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode' :: [Bit] -> String
decode' = map (chr . bin2Int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode' . channel . encode

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : filter (/= x) (uniq xs)

count :: (Eq a) => a -> [a] -> Int
count v = length . filter (== v)

result :: (Ord a) => [a] -> [(Int, a)]
result ls = sort [(count v ls, v) | v <- uniq ls]

winner :: (Ord a) => [a] -> a
winner = snd . last . result