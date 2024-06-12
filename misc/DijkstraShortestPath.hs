module Dijkstra where

import Data.Foldable
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord

data Graph v c = MkGraph
  { vertices :: [v],
    neighbors :: v -> [v],
    cost :: v -> v -> c
  }

-- dist :: Map v c --> not in map: infinity
-- prev :: Map v v --> not in map: undefined
-- q :: [v]

data Info v c = MkInfo
  { dist :: !(Map v c),
    prev :: !(Map v v)
  }
  deriving (Show)

minViewOn :: (Ord c, Eq v) => (v -> c) -> [v] -> Maybe (v, [v])
minViewOn _ [] = Nothing
minViewOn f xs =
  let m = minimumBy (comparing f) xs
   in Just (m, Data.List.delete m xs)

data Distance c
  = Finite c
  | Infinity
  deriving (Eq, Ord)

distanceOf :: (Ord v) => Info v c -> v -> Distance c
distanceOf info v =
  maybe Infinity Finite (Map.lookup v (dist info))

dijkstra ::
  forall v c.
  (Num c, Ord c, Ord v) =>
  Graph v c ->
  v ->
  Info v c
dijkstra graph source =
  let loop :: [v] -> Info v c -> Info v c
      loop q info =
        MkInfo
          { dist = Map.empty,
            prev = Map.empty
          }
   in loop
        (vertices graph)
        ( MkInfo
            { dist = Map.singleton source 0,
              prev = Map.empty
            }
        )