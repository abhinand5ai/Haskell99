halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
  where
    n = length xs `div` 2

third :: [a] -> a
third = head . tail . tail

third' xs = xs !! 2

third'' (_ : _ : x : _) = x

safetail :: [a] -> [a]
safetail xs =
  if null xs
    then []
    else tail xs

safetail' xs
  | null xs = []
  | otherwise = tail xs

safetail'' [] = []
safetail'' (_ : xs) = xs

(||) :: Bool -> Bool -> Bool
(||) False False = False
(||) _ _ = True

(|||) :: Bool -> Bool -> Bool
(|||) False b = b
(|||) True _ = True

(&&) :: Bool -> Bool -> Bool
(&&) a b =
  if a
    then if b then True else False
    else False

(&&&) :: Bool -> Bool -> Bool
(&&&) a b =
  if a
    then b
    else False

mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

luhnDouble :: Int -> Int
luhnDouble x
  | 2 * x > 9 = 2 * x - 9
  | otherwise = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum [luhnDouble a, b, luhnDouble c, d] `mod` 10 == 0

