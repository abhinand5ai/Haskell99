import Data.Graph (Tree)
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