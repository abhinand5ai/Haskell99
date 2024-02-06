abs :: (Ord a, Num a) => a -> a
abs n
  | n >= 0 = n
  | otherwise = -n

signum' :: (Ord a, Num a) => a -> a
signum' n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1