import qualified Data.Map as Map

choices :: [a] -> [[a]]
choices xs = [choice | comb <- combinations xs, choice <- permutations comb]

combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs) = cs ++ [x : comb | comb <- cs]
  where
    cs = combinations xs

interleave :: [a] -> a -> [[a]]
interleave [] x        = [[x]]
interleave (x:xs) item = (item : x : xs) : [x : sp | sp <- interleave xs item]

permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = [perm | p <- permutations xs, perm <- interleave p x]

removeFirst :: Eq a => [a] -> a -> [a]
removeFirst [] _ = []
removeFirst (x:xs) item =
  if x == item
    then xs
    else x : removeFirst xs item

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice ls []     = True
isChoice ls [x]    = x `elem` ls
isChoice ls (x:xs) = isChoice ls [x] && isChoice (removeFirst ls x) xs
