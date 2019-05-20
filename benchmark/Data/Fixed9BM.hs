module Data.Fixed9BM (benchmarks) where

import Data.Fixed (Fixed, E9)
import Data.Fixed9 (Fixed9, dblToFixed9)
import Data.Int (Int64)

import qualified Criterion as C

benchmarks :: C.Benchmark
benchmarks = C.bgroup "Fixed9"
  [ C.bgroup "Add"
    [ int64Add
    , fixed9Add2
    , e9Add
    , fixed9Add
    ]
  , C.bgroup "Multiply"
    [ int64Multi
    , e9Multi
    , fixed9Multi
    ]
  , C.bgroup "Divide"
    [ int64Divide
    , e9Divide
    , fixed9Divide
    ]
  ]

-----------------------------------
--            ADD
-----------------------------------

int64Add :: C.Benchmark
int64Add = C.bench "GHC Int64" (C.nf ((1 :: Int64) +) 1)

e9Add :: C.Benchmark
e9Add = C.bench "GHC E9 (Integer)" (C.nf ((1 :: Fixed E9) +) 1)

fixed9Add :: C.Benchmark
fixed9Add = C.bench "Fixed9" (C.nf ((1 :: Fixed9) +) 1)

fixed9Add2 :: C.Benchmark
fixed9Add2 = C.bench "Cast Fixed9" (C.nf ((fromIntegral (1 :: Fixed9) :: Int) +) 1)

-----------------------------------
--         Multiply
-----------------------------------

int64Multi :: C.Benchmark
int64Multi = C.bench "Int64" (C.nf (c *) c)
  where
    c = 15 :: Int64

e9Multi :: C.Benchmark
e9Multi = C.bench "Fixed E9 (Integer)" (C.nf (c *) c)
  where
    c = realToFrac (1500.25 :: Double) :: Fixed E9

fixed9Multi :: C.Benchmark
fixed9Multi = C.bench "Fixed9" (C.nf (c *) c)
  where
    c = dblToFixed9 1500.25

-----------------------------------
--         Divide
-----------------------------------

int64Divide :: C.Benchmark
int64Divide = C.bench "Int64" (C.nf (c *) 2.5)
  where
    c = 15.25 :: Double

e9Divide :: C.Benchmark
e9Divide = C.bench "Fixed E9 (Integer)" (C.nf (c *) 2.5)
  where
    c = realToFrac (1500.25 :: Double) :: Fixed E9

fixed9Divide :: C.Benchmark
fixed9Divide = C.bench "Fixed9" (C.nf (c /) (dblToFixed9 2.5))
  where
    c = dblToFixed9 1500.25
