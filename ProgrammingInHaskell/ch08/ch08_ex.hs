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
