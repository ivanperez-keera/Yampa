-- |
-- Description : Test cases for FRP.Yampa.Integration
-- Copyright   : (c) Ivan Perez, 2014-2022
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- Authors     : Antony Courtney, Henrik Nilsson, Ivan Perez
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
  [ testProperty "integral (0, qc)"           testIntegral0
  , testProperty "integral (1, qc)"           testIntegral1
  , testProperty "integral (2, qc)"           testIntegral2
  , testProperty "imIntegral (0, qc)"         testImIntegral0
  , testProperty "imIntegral (1, qc)"         testImIntegral1
  , testProperty "imIntegral (2, qc)"         testImIntegral2
  , testProperty "trapezoidIntegral (0, qc)"  testTrapezoidIntegral0
  , testProperty "trapezoidIntegral (1, qc)"  testTrapezoidIntegral1
  , testProperty "impulseIntegral (0, fixed)" (property $ utils_t7 ~= utils_t7r)
  , testProperty "count (0, fixed)"           (property $ utils_t4 ~= utils_t4r)
  , testProperty "count (1, fixed)"           (property $ utils_t5 ~= utils_t5r)
  , testProperty "derivative (fixed)"         (property $ der_t0_max_diff < 0.05)
  , testProperty "derivative (1, qc)"         prop_derivative_1
  , testProperty "derivative (2, qc)"         prop_derivative_2
  , testProperty "iterFrom (0, qc)"           testIterFrom0
  , testProperty "iterFrom (1, qc)"           testIterFrom1
  , testProperty "iterFrom (2, qc)"           testIterFrom2
  ]

-- * Integration

testIntegral0 :: Property
testIntegral0 =
    forAll myStream $ \s ->
      forAll number $ \n ->
        evalT (Next $ Always $ prop ((sf n &&& sfByHand n), const close)) s

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    number :: Gen Double
    number = arbitrary

    sf :: Double -> SF Time Time
    sf n = constant n >>> integral

    sfByHand :: Double -> SF Time Time
    sfByHand n = localTime >>> arr (* n)

    close (x,y) = abs (x-y) < 0.05

testIntegral1 :: Property
testIntegral1 =
    forAll myStream $ evalT $
      Next $ Always $ prop ((sf &&& sfByHand), const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    sf :: SF Double Double
    sf = integral >>> derivative

    -- Delay stream by one sample
    sfByHand = loopPre 0 (arr $ \(x, y) -> (y, x))

    close (x,y) = abs (x-y) < 0.05

testIntegral2 :: Property
testIntegral2 =
    forAll myStream $ evalT $
      Next $ Always $ prop ((sf &&& sfByHand), const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    sf :: SF Double Double
    sf = arr (*2) >>> integral

    sfByHand :: SF Double Double
    sfByHand = integral >>> arr (*2)

    close (x,y) = abs (x-y) < 0.05

testImIntegral0 :: Property
testImIntegral0 =
    forAll myStream $ \s ->
      forAll number $ \n ->
        evalT (Next $ Always $ prop ((sf n &&& sfByHand n), const close)) s

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    number :: Gen Double
    number = arbitrary

    sf :: Double -> SF Time Time
    sf n = constant n >>> imIntegral 0

    sfByHand :: Double -> SF Time Time
    sfByHand n = localTime >>> arr (* n)

    close (x,y) = abs (x-y) < 0.05

testImIntegral1 :: Property
testImIntegral1 =
    forAll myStream $ evalT $
      Next $ Always $ prop ((sf &&& sfByHand), const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    sf :: SF Double Double
    sf = imIntegral 0 >>> derivative

    sfByHand = identity

    close (x,y) = abs (x-y) < 0.05

testImIntegral2 :: Property
testImIntegral2 =
    forAll myStream $ evalT $
      Next $ Always $ prop ((sf &&& sfByHand), const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    sf :: SF Double Double
    sf = arr (*2) >>> imIntegral 0

    sfByHand :: SF Double Double
    sfByHand = imIntegral 0 >>> arr (*2)

    close (x,y) = abs (x-y) < 0.05

testTrapezoidIntegral0 :: Property
testTrapezoidIntegral0 =
    forAll myStream $ evalT $
      Next $ Always $ prop ((sf &&& sfByHand), const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    sf :: SF Double Double
    sf = trapezoidIntegral >>> derivative

    sfByHand :: SF Double Double
    sfByHand = (identity &&& iPre 0) >>^ (\(x, y) -> (x + y) / 2)

    close (x,y) = abs (x-y) < 0.05

testTrapezoidIntegral1 :: Property
testTrapezoidIntegral1 =
    forAll myStream $ evalT $
      Next $ Always $ prop ((sf &&& sfByHand), const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    sf :: SF Double Double
    sf = arr (*2) >>> trapezoidIntegral

    sfByHand :: SF Double Double
    sfByHand = trapezoidIntegral >>> arr (*2)

    close (x,y) = abs (x-y) < 0.05

utils_t7 :: [Double]
utils_t7 = take 50 $ embed impulseIntegral
                           (deltaEncode 0.1 (zip (repeat 1.0) evSeq))
  where
    evSeq = replicate 9 NoEvent ++ [Event 10.0]
            ++ replicate 9 NoEvent ++ [Event (-10.0)]
            ++ evSeq

utils_t7r =
  [  0.0,  0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8, 10.9
  , 11.0, 11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7, 11.8,  1.9
  ,  2.0,  2.1,  2.2,  2.3,  2.4,  2.5,  2.6,  2.7,  2.8, 12.9
  , 13.0, 13.1, 13.2, 13.3, 13.4, 13.5, 13.6, 13.7, 13.8,  3.9
  ,  4.0,  4.1,  4.2,  4.3,  4.4,  4.5,  4.6,  4.7,  4.8, 14.9
  ]

utils_t4 :: [Event Int]
utils_t4 = take 16 $ embed count utils_inp1

utils_t4r :: [Event Int]
utils_t4r =
  [ NoEvent, NoEvent, Event 1, NoEvent
  , Event 2, NoEvent, NoEvent, NoEvent
  , Event 3, Event 4, Event 5, NoEvent
  , Event 6, NoEvent, NoEvent, NoEvent
  ]

utils_inp1 = deltaEncode 1.0 $
  [ NoEvent,   NoEvent,   Event 1.0, NoEvent
  , Event 2.0, NoEvent,   NoEvent,   NoEvent
  , Event 3.0, Event 4.0, Event 4.0, NoEvent
  , Event 0.0, NoEvent,   NoEvent,   NoEvent
  ]
  ++ repeat NoEvent

utils_t5 :: [Event Int]
utils_t5 = take 16 $ embed count utils_inp2

utils_t5r :: [Event Int]
utils_t5r =
  [ Event 1, NoEvent, NoEvent, NoEvent
  , Event 2, NoEvent, NoEvent, NoEvent
  , Event 3, Event 4, Event 5, NoEvent
  , Event 6, NoEvent, NoEvent, NoEvent
  ]

utils_inp2 = deltaEncode 1.0 $
  [ Event 1.0, NoEvent,   NoEvent,   NoEvent
  , Event 2.0, NoEvent,   NoEvent,   NoEvent
  , Event 3.0, Event 4.0, Event 4.0, NoEvent
  , Event 0.0, NoEvent,   NoEvent,   NoEvent
  ]
  ++ repeat NoEvent

-- * Differentiation

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

  where
    myStream :: Gen (SignalSampleStream Double)
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

testIterFrom0 :: Property
testIterFrom0 =
    forAll myStream $ evalT $
      Next $ Always $ prop ((sf &&& sfByHand), const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    sf :: SF Double Double
    sf = iterFrom (\_ _ _ _ -> 0) 0

    sfByHand :: SF Double Double
    sfByHand = constant 0

    close (x,y) = abs (x-y) < 0.05

testIterFrom1 :: Property
testIterFrom1 =
    forAll myStream $ evalT $
      Next $ Always $ prop ((sf &&& sfByHand), const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    sf :: SF Double Double
    sf = iterFrom (\a0 _a1 dt b -> a0 * dt + b) 0

    sfByHand :: SF Double Double
    sfByHand = integral

    close (x,y) = abs (x-y) < 0.05

testIterFrom2 :: Property
testIterFrom2 =
    forAll myStream $ evalT $
      Next $ Always $ prop ((sf &&& sfByHand), const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

    sf :: SF Double Double
    sf = iterFrom (\a0 a1 dt _b -> (a1 - a0) / dt) 0

    sfByHand :: SF Double Double
    sfByHand = derivative

    close (x,y) = abs (x-y) < 0.05

-- * Auxiliary
-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)
