-- |
-- Description : Test cases for FRP.Yampa.InternalCore
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.InternalCore
    ( tests
    )
  where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa
import FRP.Yampa.Stream
import FRP.Yampa.QuickCheck
import FRP.Yampa.LTLFuture

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.InternalCore"
  [ testProperty "arr (0, fixed)" (property $ arr_t0 ~= arr_t0r)
  , testProperty "arr (1, fixed)" (property $ arr_t1 ~= arr_t1r)
  ]

-- * Test cases for arr

arr_t0 = testSF1 (arr (+1))
arr_t0r =
  [ 1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0
  , 17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0
  ]

arr_t1 = testSF2 (arr (+1))
arr_t1r =
  [ 1.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0
  , 4.0,4.0,5.0,5.0,5.0,5.0,5.0
  ]

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)

stepDiff :: Num a => a -> SF a a
stepDiff z = loopPre z (arr (\(x,y) -> (x - y, x)))
