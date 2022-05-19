-- |
-- Description : Test cases for FRP.Yampa.Conditional
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.Conditional
    ( tests
    )
  where

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa
import FRP.Yampa.Conditional (provided)

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Conditional"
  [ testProperty "provided (1, fixed)" (property $ utils_t8 ~= utils_t8r)
  , testProperty "provided (2, fixed)" (property $ utils_t9 ~= utils_t9r)
  ]

utils_t8 :: [Double]
utils_t8 = take 50 $ embed (provided (even . floor) integral (constant (-1)))
                           (deltaEncode 0.1 input)
  where
    input = replicate 10 1
            ++ replicate 10 2
            ++ replicate 10 3
            ++ replicate 10 4
            ++ input

utils_t8r =
  [ -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0
  ,  0.0,  0.2,  0.4,  0.6,  0.8,  1.0,  1.2,  1.4,  1.6,  1.8
  , -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0
  ,  0.0,  0.4,  0.8,  1.2,  1.6,  2.0,  2.4,  2.8,  3.2,  3.6
  , -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0
  ]

utils_t9 :: [Double]
utils_t9 = take 50 $ embed (provided (odd . floor) integral (constant (-1)))
                           (deltaEncode 0.1 input)
  where
    input = replicate 10 1
            ++ replicate 10 2
            ++ replicate 10 3
            ++ replicate 10 4
            ++ input

utils_t9r =
  [  0.0,  0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8,  0.9
  , -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0
  ,  0.0,  0.3,  0.6,  0.9,  1.2,  1.5,  1.8,  2.1,  2.4,  2.7
  , -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0
  ,  0.0,  0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8,  0.9
  ]
