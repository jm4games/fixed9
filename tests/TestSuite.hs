module Main where

import Test.HUnit.Text (runTestTT)
import Test.QuickCheck (quickCheck)

import qualified Data.Fixed9Tests

main :: IO ()
main = do
  runTestTT Data.Fixed9Tests.tests
  quickCheck Data.Fixed9Tests.properties
