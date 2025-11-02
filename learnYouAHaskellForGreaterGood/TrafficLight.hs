module TrafficLight
  ( TrafficLight(..)
  , EEq(..)
  , YesNo
  , yesnoIf
  ) where

import           Tree

class EEq a where
  (~~) :: a -> a -> Bool
  (!~) :: a -> a -> Bool
  x ~~ y = not (x !~ y)
  x !~ y = not (x ~~ y)

data TrafficLight
  = Red
  | Yellow
  | Green

instance EEq TrafficLight where
  Red ~~ Red       = True
  Green ~~ Green   = True
  Yellow ~~ Yellow = True
  _ ~~ _           = False

instance Eq TrafficLight where
  Red == Red       = True
  Green == Green   = True
  Yellow == Yellow = True
  _ == _           = False

instance Show TrafficLight where
  show Red    = "Red Light"
  show Yellow = "Yellow Light"
  show Green  = "Green Light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _         = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal
    then yesResult
    else noResult
