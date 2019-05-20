module Data.C_API where

import Data.Int (Int64)

foreign import ccall unsafe "fixed9.h fixed9_fixed9Multiply"
  fixed9Multiply :: Int64 -> Int64 -> Int64

foreign import ccall unsafe "fixed9.h fixed9_fixed9Divide"
  fixed9Divide :: Int64 -> Int64 -> Int64
