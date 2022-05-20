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
  [ testProperty "arr (0, fixed)"         (property $ arr_t0 ~= arr_t0r)
  , testProperty "arr (1, fixed)"         (property $ arr_t1 ~= arr_t1r)
  , testProperty "composition (0, fixed)" (property $ comp_t0 ~= comp_t0r)
  , testProperty "composition (1, fixed)" (property $ comp_t1 ~= comp_t1r)
  , testProperty "composition (2, fixed)" (property $ comp_t2 ~= comp_t2r)
  , testProperty "composition (3, fixed)" (property $ comp_t3 ~= comp_t3r)
  , testProperty "composition (4, fixed)" (property $ comp_t4 ~= comp_t4r)
  , testProperty "composition (5, fixed)" (property $ comp_t5 ~= comp_t5r)
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

-- * Test cases for comp

comp_t0 = testSF1 ((arr (+1)) >>> (arr (+2)))
comp_t0r :: [Double]
comp_t0r =
  [ 3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0
  , 18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0
  ]

comp_t1 = testSF2 ((arr (+1)) >>> (arr (+2)))
comp_t1r :: [Double]
comp_t1r =
  [ 3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,5.0
  , 6.0,6.0,6.0,6.0,6.0,7.0,7.0,7.0,7.0,7.0
  ]

comp_t2 = testSF1 ((constant 5.0) >>> (arr (+1)))
comp_t2r :: [Double]
comp_t2r =
  [ 6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0
  , 6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0
  ]

comp_t3 = testSF2 ((constant 5.0) >>> (arr (+1)))
comp_t3r :: [Double]
comp_t3r =
  [ 6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0
  , 6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0
  ]

-- Integration by the rectangle rule or trapezoid rule makes no difference.
comp_t4 = testSF1 ((constant 2.0) >>> integral)
comp_t4r :: [Double]
comp_t4r =
  [ 0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5
  , 9.0,9.5,10.0,10.5,11.0,11.5,12.0
  ]

-- Same result as above.
comp_t5 = testSF2 ((constant 2.0) >>> integral)
comp_t5r :: [Double]
comp_t5r =
  [ 0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5
  , 9.0,9.5,10.0,10.5,11.0,11.5,12.0
  ]

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)

stepDiff :: Num a => a -> SF a a
stepDiff z = loopPre z (arr (\(x,y) -> (x - y, x)))
