module Main where

import qualified Criterion.Main as C

import qualified Data.Fixed9BM

main :: IO ()
main = C.defaultMain [Data.Fixed9BM.benchmarks]
