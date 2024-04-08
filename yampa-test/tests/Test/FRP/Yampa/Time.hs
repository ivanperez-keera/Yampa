-- |
-- Description : Test cases for FRP.Yampa.Time
-- Copyright   : (c) Ivan Perez, 2014-2022
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- Authors     : Antony Courtney, Henrik Nilsson, Ivan Perez
module Test.FRP.Yampa.Time
    ( tests
    )
  where

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa
import FRP.Yampa.Stream
import FRP.Yampa.QuickCheck
import FRP.Yampa.LTLFuture

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Time"
  [ testProperty "localTime (fixed)"               (property $ basicsf_t2 ~= basicsf_t2r)
  , testProperty "Basic > localTime"               prop_basic_localtime_increasing
  , testProperty "time (fixed)"                    (property $ basicsf_t3 ~= basicsf_t3r)
  , testProperty "Basic > Time"                    prop_basic_time_increasing
  , testProperty "Basic > Time (fixed delay)"      prop_basic_time_fixed_delay
  , testProperty "Basic > localTime (fixed delay)" prop_basic_localtime_fixed_delay
  ]

basicsf_t2 :: [Double]
basicsf_t2 = testSF1 localTime
basicsf_t2r =
  [ 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25
  , 2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0, 4.25, 4.5, 4.75
  , 5.0, 5.25, 5.5, 5.75, 6.0
  ]

prop_basic_localtime_increasing =
    forAll myStream $ evalT $ Always $ prop (sf, const (uncurry (>)))
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    sf   :: SF a (Time, Time)
    sf   = loopPre (-1 :: Time) sfI

    sfI :: SF (a,Time) ((Time, Time), Time)
    sfI =  (localTime *** identity) >>> arr resort

    resort :: (Time, Time) -> ((Time,Time),Time)
    resort (newT, oldT) = ((newT, oldT), newT)

basicsf_t3 :: [Double]
basicsf_t3 = testSF1 time
basicsf_t3r =
  [ 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25
  , 2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0, 4.25, 4.5, 4.75
  , 5.0, 5.25, 5.5, 5.75, 6.0
  ]

-- | Starting with an accumulator of -1, it gets the local
--   time and outputs the time and the accumulator, updating
--   the latter with the local time at every iteration.
--   The predicate checks whether the time is always strictly
--   greater than the acc.
prop_basic_time_increasing =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    sf   :: SF a (Time, Time)
    sf   = loopPre (-1 :: Time) sfI

    sfI :: SF (a,Time) ((Time, Time), Time)
    sfI =  (time *** identity) >>> arr resort

    resort :: (Time, Time) -> ((Time,Time),Time)
    resort (newT, oldT) = ((newT, oldT), newT)

    pred :: a -> (Time, Time) -> Bool
    pred _ (t,o) = (t > o)

prop_basic_time_fixed_delay =
    forAll myStream $ evalT $
      Always (prop (sf25msec, const (== d)))

  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = fixedDelayStream d

    sf25msec = time >>> stepDiff (-d)

    d :: Time
    d = 0.25

prop_basic_localtime_fixed_delay =
    forAll myStream $ evalT $
      Always (prop (sf25msec, const (== d)))

  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = fixedDelayStream d

    sf25msec = time >>> stepDiff (-d)

    d :: Time
    d = 0.25

-- * Auxiliary

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)

stepDiff :: Num a => a -> SF a a
stepDiff z = loopPre z (arr (\(x,y) -> (x - y, x)))
