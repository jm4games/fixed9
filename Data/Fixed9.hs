{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Fixed9
  ( Fixed9(..)
  , e9ToFixed9
  , dblToFixed9
  , truncateTo
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (liftM)

import Data.Fixed (Fixed(..), E9)
import Data.Hashable (Hashable(..))
import Data.Int (Int64)
import Data.Primitive.Types (Prim(..))
import Data.Ratio (Ratio, numerator, denominator)
import Data.Vector.Unboxed (MVector, Vector, Unbox)

import GHC.Real ((%), divZeroError, overflowError)

import Numeric (showFFloat)

import TextShow (TextShow(..))
import TextShow.Data.Floating (showbFFloat)

import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import qualified Data.C_API as CAPI

magnitude :: Int64
{-# INLINE magnitude #-}
magnitude = 1000000000

newtype Fixed9 = Fixed9 { unwrapFixed9 :: Int64 } deriving (Eq, Ord)

instance NFData Fixed9 where
  rnf (Fixed9 x) = rnf x

instance Bounded Fixed9 where
  minBound = Fixed9 minBound
  maxBound = Fixed9 maxBound

instance Enum Fixed9 where
  toEnum = Fixed9 . fromInteger .toEnum
  fromEnum =  fromEnum . unwrapFixed9

instance Real Fixed9 where
  toRational (Fixed9 x) = fromIntegral x % 1000000000

instance Fractional Fixed9 where
  (/) (Fixed9 a) (Fixed9 b)
    | b == 0 = divZeroError
    | otherwise = Fixed9 (CAPI.fixed9Divide a b)
  -- FIXME: Bounds check
  fromRational r = Fixed9 . fromInteger $
    (numerator r * 1000000000) `quot` denominator r

instance RealFrac Fixed9 where
  properFraction a =
    case truncate (toRational a) of
      x -> (x, a - fromIntegral x)
  truncate f = truncate (toRational f)
  round f = round (toRational f)
  ceiling f = ceiling (toRational f)
  floor f = floor (toRational f)

instance Num Fixed9 where
  (Fixed9 a) + (Fixed9 b) = Fixed9 (a + b)
  (Fixed9 a) * (Fixed9 b) = Fixed9 (CAPI.fixed9Multiply a b)
  (Fixed9 a) - (Fixed9 b) = Fixed9 (a - b)
  abs = Fixed9 . abs . unwrapFixed9
  signum = Fixed9 . signum . unwrapFixed9
  fromInteger = Fixed9 . (*) magnitude . fromInteger

instance Integral Fixed9 where
  quot (Fixed9 a) (Fixed9 b)
    | b == 0 = divZeroError
    | b == (-1) && a == minBound = overflowError
    | otherwise = Fixed9 ((CAPI.fixed9Divide a b `quot` magnitude) * magnitude)
  rem (Fixed9 a) (Fixed9 b)
    | b == 0 = divZeroError
    | b == (-1) = 0
    -- NOT ideal implementation, but it will do for now
    | otherwise = Fixed9 (((a `quot` magnitude) `rem` (b `quot` magnitude)) * magnitude)
  div (Fixed9 a) (Fixed9 b)
    | b == 0 = divZeroError
    | b == (-1) && a == minBound = overflowError
    | otherwise = Fixed9 ((CAPI.fixed9Divide a b `div` magnitude) * magnitude)
  mod (Fixed9 a) (Fixed9 b)
    | b == 0 = divZeroError
    | b == (-1) = 0
    -- NOT ideal implementation, but it will do for now
    | otherwise = Fixed9 (((a `div` magnitude) `mod` (b `div` magnitude)) * magnitude)
  quotRem (Fixed9 a) (Fixed9 b)
    | b == 0 = divZeroError
    | b == (-1) && a == minBound = (overflowError, 0)
    | otherwise = ( Fixed9 ((CAPI.fixed9Divide a b `quot` magnitude) * magnitude)
                  , Fixed9 (((a `quot` magnitude) `rem` (b `quot` magnitude)) * magnitude)
                  )
  divMod (Fixed9 a) (Fixed9 b)
    | b == 0 = divZeroError
    | b == (-1) && a == minBound = (overflowError, 0)
    | otherwise = ( Fixed9 ((CAPI.fixed9Divide a b `div` magnitude) * magnitude)
                  , Fixed9 (((a `div` magnitude) `mod` (b `div` magnitude)) * magnitude)
                  )
  toInteger (Fixed9 a) = toInteger (a `quot` magnitude)

instance Hashable Fixed9 where
  hashWithSalt s (Fixed9 i) = hashWithSalt s i
  hash (Fixed9 i) = hash i

truncateTo :: Fixed9 -> Int64 -> Fixed9
truncateTo a@(Fixed9 x) i
  | i > 0 && i < 9 = Fixed9 ((x `quot` prec) * prec)
  | i == 0 = floor a
  | otherwise = a
  where
    prec = i * 10

instance Show Fixed9 where
  show v = showFFloat Nothing (realToFrac v :: Double) ""

instance TextShow Fixed9 where
  showb v = showbFFloat Nothing (realToFrac v :: Double)

instance Prim Fixed9 where
  {-# INLINE sizeOf# #-}
  sizeOf# (Fixed9 a)= sizeOf# a
  {-# INLINE alignment# #-}
  alignment# (Fixed9 a)= alignment# a
  {-# INLINE indexByteArray# #-}
  indexByteArray# b i = Fixed9 (indexByteArray# b i)
  {-# INLINE readByteArray# #-}
  readByteArray# mba i st =
   case readByteArray# mba i st of
     (# s, a #) -> (# s, Fixed9 a #)
  {-# INLINE writeByteArray# #-}
  writeByteArray# mba i (Fixed9 a) = writeByteArray# mba i a
  {-# INLINE setByteArray# #-}
  setByteArray# mba x y (Fixed9 a) = setByteArray# mba x y a
  {-# INLINE indexOffAddr# #-}
  indexOffAddr# addr i = Fixed9 (indexOffAddr# addr i)
  {-# INLINE readOffAddr# #-}
  readOffAddr# addr i st =
   case readOffAddr# addr i st of
     (# s, a #) -> (# s, Fixed9 a #)
  {-# INLINE writeOffAddr# #-}
  writeOffAddr# addr i (Fixed9 a) = writeOffAddr# addr i a
  {-# INLINE setOffAddr# #-}
  setOffAddr# addr x y (Fixed9 a) = setOffAddr# addr x y a

newtype instance MVector s Fixed9 = MV_Fixed9 (VP.MVector s Fixed9)
newtype instance Vector    Fixed9 = V_Fixed9  (VP.Vector    Fixed9)

instance VGM.MVector MVector Fixed9 where
  {-# INLINE basicLength #-}
  basicLength (MV_Fixed9 v) = VGM.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (MV_Fixed9 v) = MV_Fixed9 $ VGM.basicUnsafeSlice i n v
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Fixed9 v1) (MV_Fixed9 v2) = VGM.basicOverlaps v1 v2
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = MV_Fixed9 `liftM` VGM.basicUnsafeNew n
  {-# INLINE basicInitialize #-}
  basicInitialize (MV_Fixed9 v) = VGM.basicInitialize v
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n a = MV_Fixed9 `liftM` VGM.basicUnsafeReplicate n a
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Fixed9 v) = VGM.basicUnsafeRead v
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Fixed9 v) = VGM.basicUnsafeWrite v
  {-# INLINE basicClear #-}
  basicClear (MV_Fixed9 v) = VGM.basicClear v
  {-# INLINE basicSet #-}
  basicSet (MV_Fixed9 v) = VGM.basicSet v
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Fixed9 v1) (MV_Fixed9 v2) = VGM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MV_Fixed9 v1) (MV_Fixed9 v2) = VGM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MV_Fixed9 v) n = MV_Fixed9 `liftM` VGM.basicUnsafeGrow v n

instance VG.Vector Vector Fixed9 where
  {-# INLINE basicLength #-}
  basicLength (V_Fixed9 v) = VG.basicLength v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Fixed9 v) = V_Fixed9 `liftM` VG.basicUnsafeFreeze v
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Fixed9 v) = MV_Fixed9 `liftM` VG.basicUnsafeThaw v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (V_Fixed9 v) = V_Fixed9 $ VG.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Fixed9 v) = VG.basicUnsafeIndexM v
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Fixed9 mv) (V_Fixed9 v) = VG.basicUnsafeCopy mv v
  {-# INLINE elemseq #-}
  elemseq _ = seq

instance Unbox Fixed9 where

-- FIXME: Bounds check
dblToFixed9 :: Double -> Fixed9
dblToFixed9 x = Fixed9 (round $ x * 1000000000)

e9ToFixed9 :: Fixed E9 -> Fixed9
e9ToFixed9 (MkFixed x) = Fixed9 (fromIntegral x)
