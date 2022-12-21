module TS
  ( tsp,
  )
where

import Data.List (minimumBy, permutations)
import Data.Ord (comparing)
import Utils (Coordinate, cost)

-- get the shortest path
tsp :: [Coordinate] -> [Coordinate]
tsp coordinates = minimumBy (comparing cost) (permutations coordinates)
