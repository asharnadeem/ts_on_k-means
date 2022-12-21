{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import Lib (runTsk)

main :: IO ()
main = do
  runTsk
