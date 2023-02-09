module Shapes
  ( Point (..),
    Shape (..),
    area,
    nudge,
    baseCircle,
    baseRect,
  )
where

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r * r
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) * (y1 - y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x1 y1) r) a b = Circle (Point (x1 + a) (y1 + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

circles = map (Circle $ Point 1 2) [3, 4, 5, 6]

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)