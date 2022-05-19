-- |
-- Description : Test cases for FRP.Yampa.Integration
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.Integration
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
tests = testGroup "Regression tests for FRP.Yampa.Integration"
  [ testProperty "derivative (fixed)" (property $ der_t0_max_diff < 0.05)
  , testProperty "derivative (1, qc)" prop_derivative_1
  , testProperty "derivative (2, qc)" prop_derivative_2
  ]

-- * Test cases for derivative

der_step = 0.001
der_N = 1000

der_t0 :: [Double]
der_t0 = take der_N $  -- First value is always 0
         embed derivative
               (deltaEncode der_step
                            [sin(2 * pi * t) | t <- [0.0, der_step ..]])
{-
-- For stepsize 0.1
der_t0r :: [Double]
der_t0r =
  [  0.0000,  5.8779,  3.6327, 0.0000, -3.6327
  , -5.8779, -5.8779, -3.6327, 0.0000,  3.6327
  ,  5.8779,  5.8779,  3.6327, 0.0000, -3.6327
  , -5.8779, -5.8779, -3.6327, 0.0000,  3.6327
  ]
-}

der_t0r :: [Double]
der_t0r = take der_N $
          [2 * pi * cos (2 * pi * t) | t <- [0.0, der_step ..]]

-- We're happy if we are in the right ball park.
der_t0_max_diff = (maximum (zipWith (\x y -> abs (x - y))
                                    (tail der_t0)
                                    (tail der_t0r)))

prop_derivative_1 =
    forAll myStream $ evalT $
      Next $ Always $ prop ((sfDer &&& sfDerByHand), const close)

  where myStream :: Gen (SignalSampleStream Double)
        myStream = fixedDelayStreamWith (\t -> sin(2 * pi * t)) der_step

        sfDer :: SF Time Time
        sfDer = derivative

        sfDerByHand = localTime >>> arr (\t -> (2 * pi * cos (2 * pi * t)))

        close (x,y) = abs (x-y) < 0.05

prop_derivative_2 =
    forAll myStream $ evalT $
      Next $ Always $ prop ( sfDer &&& sfDerByHand
                           , const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = fixedDelayStream der_step

    sfDer :: SF Time Time
    sfDer = localTime
              >>> arr (\t -> sin(2*pi*t))
                >>> derivative

    sfDerByHand = localTime
                    >>> arr (\t -> 2*pi*cos (2*pi*t))

    close (x,y) = abs (x-y) < 0.05

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)
