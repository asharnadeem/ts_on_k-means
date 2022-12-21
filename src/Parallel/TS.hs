module Parallel.TS
  ( tsp,
  )
where

import Control.Parallel.Strategies (parList, rpar, using)
import Data.List (minimumBy, permutations)
import Data.Ord (comparing)
import Utils (Coordinate, cost)

tsp :: [Coordinate] -> [Coordinate]
tsp coordinates = minimumBy (comparing cost) (permutations coordinates `using` parList rpar)
