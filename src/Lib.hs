module Lib (runTsk) where

import Control.Monad (unless, when)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Map (toList)
import KMeans (kmeans)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import TS (tsp)
import Utils (stateCapitals)

runTsk :: IO ()
runTsk = do
  program <- getProgName
  programArgs <- getArgs
  when (length programArgs /= 2) $
    die ("usage: " ++ program ++ " <strategy> <num_clusters>")
  let (strategy : rawK : _) = programArgs
  let k = read rawK
  unless (strategy == "s" || strategy == "p") $
    die ("usage: " ++ program ++ " <strategy> <num_clusters>")
  let clusters = toList $ kmeans (take k stateCapitals) stateCapitals
  if strategy == "s"
    then do
      let shortestPaths = map (tsp . snd) clusters
      mapM_ print shortestPaths
    else do
      let shortestPaths = parMap rpar (tsp . snd) clusters
      mapM_ print shortestPaths
