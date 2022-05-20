-- |
-- Description : Test cases for FRP.Yampa.Delays
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.Delays
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

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Delays"
  [ testProperty "delay (zero delay, qc)"  prop_delay_1
  , testProperty "delay (small delay, qc)" prop_delay_2
  ]

-- Delaying

-- | Delaying by 0.0 has no effect
prop_delay_1 =
    forAll myStream $ evalT $ prop_always_equal sfDelayed sf
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

        sfDelayed = delay 0.0 undefined >>> sf
        sf = arr (+1)

-- | Delaying input signal by a small amount will fill in the "blank" signal
--   with the given value, which will become also the sample at the initial
--   time.
prop_delay_2 =
    forAll myStream $ evalT $
      (prop (sfDelayed, (\x y -> y == initialValue)))
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

        sfDelayed = delay 0.0001 initialValue

        initialValue = 17

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)

-- | Compares two SFs, resulting in true if they are always equal
prop_always_equal sf1 sf2 =
    Always $ SP ((sf1 &&& sf2) >>> arr sameResult)
  where sameResult = uncurry (==)
