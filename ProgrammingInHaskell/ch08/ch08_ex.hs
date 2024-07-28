import Control.Monad (when)
import Data.Graph (Tree)
import Distribution.Backpack.ModSubst (ModSubst)
import Distribution.PackageDescription (ConfVar (Impl))
import GHC.Exts.Heap (GenClosure (var))

type Pair a = (a, a)

mult :: Pair Int -> Int
mult (x, y) = x * y

type Pos = Pair Int

data Direction = North | South | East | West deriving (Show)

move :: Direction -> Pos -> Pos
move dir (x, y) = case dir of
  North -> (x + 1, y)
  South -> (x - 1, y)
  East -> (x, y + 1)
  West -> (x, y - 1)

rev :: Direction -> Direction
rev dir = case dir of
  North -> South
  South -> North
  East -> West
  West -> East

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x : xs) = Just x

newtype Natural = N Int

data Nat = Zero | Succ Nat

data Lst a = Nil | Cons a (Lst a)

data Tree' a = Leaf a | Node (Tree' a) a (Tree' a)

----------------
-- Tautolgy eval
----------------

data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : [y | y <- xs, y /= x]

find :: (Eq k) => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k' == k]

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n =
  map (False :) bss ++ map (True :) bss
  where
    bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-----------------------------------------------

-------------------
-- Abstract Machine
-------------------

data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val x) = x
value (Add a b) = value a + value b

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eva :: Expr -> Cont -> Int
eva (Val n) c = exec c n
eva (Add x y) c = eva x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eva y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

-- 8.9
-- 1
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

mult' :: Nat -> Nat -> Nat
mult' Zero _ = Zero
mult' (Succ n) m = add (mult' n m) n

-- 2

occurs :: (Ord a) => a -> Tree' a -> Bool
occurs item tree = case tree of
  Leaf x -> EQ == compare item x
  Node left x right -> (item == x) || (occurs item left || occurs item right)

isBalanced :: Tree' a -> Bool
isBalanced (Leaf _) = True
isBalanced (Node left _ right) = abs (height left - height right) <= 1 && isBalanced left && isBalanced right

height :: Tree' a -> Int
height (Leaf _) = 1
height (Node left _ right) = 1 + max (height left) (height right)

balance :: [a] -> Tree' a
balance [x] = Leaf x
balance xs = Node (balance left) x (balance right)
  where
    (left, x : right) = splitAt (length xs `div` 2) xs

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

countExpVal :: Expr -> Int
countExpVal = folde (const 1) (+)
