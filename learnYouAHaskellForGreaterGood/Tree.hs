module Tree
  ( Tree(..)
  , treeInsert
  , singleton
  , treeElem
  ) where

data Tree a
  = EmptyTree
  | Node a (Tree a) (Tree a)
  deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
  | x == y = Node y left right
  | x < y = Node y (treeInsert x left) right
  | x > y = Node y left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
  | x == y = True
  | x < y = treeElem x left
  | x > y = treeElem x right

instance Functor Tree where
  fmap f EmptyTree           = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
