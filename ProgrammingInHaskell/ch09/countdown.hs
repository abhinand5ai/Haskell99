-- using the numbers 1 3 7 10 25 50
-- and the arithmetic operations + - * /
-- construct and expression whose value in 765
-- all numbers including intermediate results must be positive naturals
-- each source number can only be used at most once

-- one sol --> (25 - 10) * (50 + 1)

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) =
    brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) =
  [ apply o x y
    | x <- eval l,
      y <- eval r,
      valid o x y
  ]

subs :: [a] -> [[a]]
subs [] = []
subs [x] = [[], [x]]
subs (x : xs) = chs ++ [x : ch | ch <- chs]
  where
    chs = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x : xs) = concatMap (interleave x) (perms xs)

choices :: [a] -> [[a]]
choices = concatMap perms . subs
