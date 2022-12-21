module KMeans
  ( kmeans,
  )
where

import Data.List (minimumBy, sort, transpose)
import Data.Map
  ( Map,
    empty,
    foldrWithKey,
    fromListWith,
    insert,
    keys,
  )
import Data.Ord (comparing)
import Utils (Coordinate, distance, marginOfError)

-- assign coordinate to a cluster
assign :: [Coordinate] -> [Coordinate] -> Map Coordinate [Coordinate]
assign centroids coordinates = fromListWith (++) [(assignCoordinate c, [c]) | c <- coordinates]
  where
    assignCoordinate c = minimumBy (comparing (distance c)) centroids

-- recalculate clusters
recalculate :: Map Coordinate [Coordinate] -> Map Coordinate [Coordinate]
recalculate = foldrWithKey insertCenter empty
  where
    insertCenter _ xs = insert (center xs) xs
    center [] = [0, 0]
    center ps = map average (transpose ps)
    average xs = sum xs / fromIntegral (length xs)

-- run kmeans until centroids no longer move within margin of error
kmeans :: [Coordinate] -> [Coordinate] -> Map Coordinate [Coordinate]
kmeans centroids coordinates =
  if converged
    then clusters
    else kmeans (keys clusters) coordinates
  where
    converged = all (< marginOfError) $ zipWith distance (sort centroids) (keys clusters)
    clusters = recalculate (assign centroids coordinates)
