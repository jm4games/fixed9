module Data.Fixed9Tests (tests, properties) where

import Data.Fixed (Fixed, E9)
import Data.Fixed9

import Test.HUnit.Base (Test(..), Assertion, assertEqual)
import Test.QuickCheck ((===), (.&&.), (==>), Property, counterexample, conjoin, label)

testCase :: String -> Assertion -> Test
testCase lbl = TestLabel lbl . TestCase

tests :: Test
tests = TestLabel "Data.Fixed9" $ TestList
  [test1, test2, test3, test4, test5, test6]

test1 :: Test
test1 = testCase "Precision is preserved when mulitplying fixed 9 types" test
  where
    test = assertEqual "Multi Result" (dblToFixed9 232.5625) (15.25 * 15.25)

test2 :: Test
test2 = testCase "Precision is preserved when dividing fixed 9 types" test
  where
    test = assertEqual "Division Result" (dblToFixed9 6.1) (15.25 / 2.5)

test3 :: Test
test3 = testCase "Precision is preserved when adding  fixed 9 types" test
  where
    test = assertEqual "Addition Result" (dblToFixed9 30.50) (15.25 + 15.25)

test4 :: Test
test4 = testCase "Precision is preserved when subtracting fixed 9 types" test
  where
    test = assertEqual "Subtraction Result" (dblToFixed9 11.0) (15.25 - 4.25)

test5 :: Test
test5 = testCase "Fixed E9 to Fixed9 type" test
  where
    test = assertEqual "Fixed9 Conversion" (11 :: Fixed9) (e9ToFixed9 (11 :: Fixed E9))

test6 :: Test
test6 = testCase "A value of 0.003 when dividing 0.3 by 100" test
  where
    test = assertEqual "Division Result" (dblToFixed9 0.003) (0.3 / 100)

properties :: Property
properties = label "Data.Fixed9" $ conjoin [prop1, prop2]

prop1 :: Property
prop1 = label "Fixed9 Multiply" prop
  where
    prop :: Double -> Double -> Property
    prop a b =
      let a1 = dblToFixed9 a
          b1 = dblToFixed9 b
          expected = dblToFixed9 $ (realToFrac a1 :: Double) * (realToFrac b1 :: Double)
          actual = a1 * b1
          -- doubles cannot perfectly represents all fracs, so we allow for a margin of error
          marginOfError = dblToFixed9 0.000000005
          offset = actual - expected
      in     counterexample (show offset <> " < " <> show marginOfError) (abs offset <= marginOfError)
        .&&. (expected + offset) === actual

prop2 :: Property
prop2 = label "Fixed9 Divide" prop
  where
    prop :: Double -> Double -> Property
    prop a b = b /= 0 ==>
      let a1 = dblToFixed9 a
          b1 = dblToFixed9 b
          expected = dblToFixed9 $ (realToFrac a1 :: Double) / (realToFrac b1 :: Double)
          actual = a1 / b1
          -- doubles cannot perfectly represents all fracs, so we allow for a margin of error
          marginOfError = dblToFixed9 0.000000005
          offset = actual - expected
      in     counterexample (show offset <> " < " <> show marginOfError) (abs offset <= marginOfError)
        .&&. (expected + offset) === actual
