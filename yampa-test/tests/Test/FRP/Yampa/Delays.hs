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

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Delays"
  [ testProperty "iPre (0, fixed)"         (property $ pre_t0 ~= pre_t0r)
  , testProperty "iPre (1, fixed)"         (property $ pre_t1 ~= pre_t1r)
  , testProperty "iPre (2, fixed)"         (property $ pre_t2 ~= pre_t2r)
  , testProperty "iPre (3, fixed)"         (property $ pre_t3 == pre_t3r)
  , testProperty "iPre (4, fixed)"         (property $ pre_t4 == pre_t4r)
  , testProperty "iPre (5, fixed)"         (property $ pre_t5 == pre_t5r)
  , testProperty "iPre (6, fixed)"         (property $ pre_t6 == pre_t6r)
  , testProperty "iPre (7, fixed)"         (property $ pre_t7 == pre_t7r)
  , testProperty "iPre (8, fixed)"         (property $ pre_t8 == pre_t8r)
  , testProperty "delay (0, fixed)"        (property $ delay_t0 ~= delay_t0r)
  , testProperty "delay (1, fixed)"        (property $ delay_t1 ~= delay_t1r)
  , testProperty "delay (2, fixed)"        (property $ delay_t2 ~= delay_t2r)
  , testProperty "delay (3, fixed)"        (property $ delay_t3 ~= delay_t3r)
  , testProperty "delay (4, fixed)"        (property $ delay_t4 == delay_t4r)
  , testProperty "delay (5, fixed)"        (property $ delay_t5 ~= delay_t5r)
  , testProperty "delay (zero delay, qc)"  prop_delay_1
  , testProperty "delay (small delay, qc)" prop_delay_2
  ]

-- * Test cases for pre and related combinators

pre_t0 = testSF1 (iPre 17)
pre_t0r =
  [ 17.0,0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0
  , 15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0
  ]

pre_t1 = testSF2 (iPre 17)
pre_t1r =
  [ 17.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,2.0
  , 3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0
  ]

pre_t2 = testSF1 (time
                  >>> arr (\t -> sin (0.5 * t * pi + pi))
                  >>> loop (arr (\(x1,x2) -> let x' = max x1 x2 in (x',x'))
                            >>> second (iPre 0.0)))

pre_t2r =
  take 25
       (let xs = [ sin (0.5 * t * pi + pi) | t <- [0.0, 0.25 ..] ]
        in tail (scanl max 0 xs))

-- This is a (somewhat strange) way of doing a counter that
-- stops after reaching a threshold. Note that the ingoing event
-- is *control dependent* on the output of the counter, so
-- "dHold" really has to have the capability of delivering an
-- output without looking at the current input at all.
pre_t3, pre_t3r :: [Int]
pre_t3 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
  where
    sf = repeatedly 1.0 ()
         >>> (loop $
                arr (\(e,c) -> (e `tag` (c + 1)) `gate` (c < 10))
                >>> dHold 0
                >>> arr dup)
pre_t3r = [ 0,0,0,0      -- 0s
          , 0,1,1,1      -- 1s
          , 1,2,2,2      -- 2s
          , 2,3,3,3      -- 3s
          , 3,4,4,4      -- 4s
          , 4,5,5,5      -- 5s
          , 5,6,6,6      -- 6s
          , 6,7,7,7      -- 7s
          , 7,8,8,8      -- 8s
          , 8,9,9,9      -- 9s
          , 9,10,10,10   -- 10s
          , 10,10,10,10  -- 11s
          , 10,10        -- 12s
          ]

-- Version of the above that tests that thigs still work OK also if
-- there is an initial event.
pre_t4, pre_t4r :: [Int]
pre_t4 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
  where
    sf = (now () &&& repeatedly 1.0 ()) >>> arr (uncurry lMerge)
         >>> (loop $
                arr (\(e,c) -> (e `tag` (c + 1)) `gate` (c < 10))
                >>> dHold 0
                >>> arr dup)
pre_t4r = [ 0,1,1,1      -- 0s
          , 1,2,2,2      -- 1s
          , 2,3,3,3      -- 2s
          , 3,4,4,4      -- 3s
          , 4,5,5,5      -- 4s
          , 5,6,6,6      -- 5s
          , 6,7,7,7      -- 6s
          , 7,8,8,8      -- 7s
          , 8,9,9,9      -- 8s
          , 9,10,10,10   -- 9s
          , 10,10,10,10  -- 10s
          , 10,10,10,10  -- 11s
          , 10,10        -- 12s
          ]

-- Similar test to "pre_t3" above but for dAccumHold.
pre_t5, pre_t5r :: [Int]
pre_t5 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
  where
    sf = repeatedly 1.0 ()
         >>> (loop $
                arr (\(e,c) -> (e `tag` (+1)) `gate` (c < 10))
                >>> dAccumHold 0
                >>> arr dup)
pre_t5r = [ 0,0,0,0      -- 0s
          , 0,1,1,1      -- 1s
          , 1,2,2,2      -- 2s
          , 2,3,3,3      -- 3s
          , 3,4,4,4      -- 4s
          , 4,5,5,5      -- 5s
          , 5,6,6,6      -- 6s
          , 6,7,7,7      -- 7s
          , 7,8,8,8      -- 8s
          , 8,9,9,9      -- 9s
          , 9,10,10,10   -- 10s
          , 10,10,10,10  -- 11s
          , 10,10        -- 12s
          ]

-- Similar test to "pre_t4" above but for dAccumHold.
pre_t6, pre_t6r :: [Int]
pre_t6 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
  where
    sf = (now () &&& repeatedly 1.0 ()) >>> arr (uncurry lMerge)
         >>> (loop $
                arr (\(e,c) -> (e `tag` (+1)) `gate` (c < 10))
                >>> dAccumHold 0
                >>> arr dup)
pre_t6r = [ 0,1,1,1      -- 0s
          , 1,2,2,2      -- 1s
          , 2,3,3,3      -- 2s
          , 3,4,4,4      -- 3s
          , 4,5,5,5      -- 4s
          , 5,6,6,6      -- 5s
          , 6,7,7,7      -- 6s
          , 7,8,8,8      -- 7s
          , 8,9,9,9      -- 8s
          , 9,10,10,10   -- 9s
          , 10,10,10,10  -- 10s
          , 10,10,10,10  -- 11s
          , 10,10        -- 12s
          ]

-- Similar test to "pre_t3" above but for dAccumHoldBy.
pre_t7, pre_t7r :: [Int]
pre_t7 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
  where
    sf = repeatedly 1.0 ()
         >>> (loop $
                arr (\(e,c) -> e `gate` (c < 10))
                >>> dAccumHoldBy (\c _ -> c + 1) 0
                >>> arr dup)
pre_t7r = [ 0,0,0,0      -- 0s
          , 0,1,1,1      -- 1s
          , 1,2,2,2      -- 2s
          , 2,3,3,3      -- 3s
          , 3,4,4,4      -- 4s
          , 4,5,5,5      -- 5s
          , 5,6,6,6      -- 6s
          , 6,7,7,7      -- 7s
          , 7,8,8,8      -- 8s
          , 8,9,9,9      -- 9s
          , 9,10,10,10   -- 10s
          , 10,10,10,10  -- 11s
          , 10,10        -- 12s
          ]

-- Similar test to "pre_t4" above but for dAccumHoldBy.
pre_t8, pre_t8r :: [Int]
pre_t8 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
  where
    sf = (now () &&& repeatedly 1.0 ()) >>> arr (uncurry lMerge)
         >>> (loop $
                arr (\(e,c) -> e `gate` (c < 10))
                >>> dAccumHoldBy (\c _ -> c + 1) 0
                >>> arr dup)
pre_t8r = [ 0,1,1,1      -- 0s
          , 1,2,2,2      -- 1s
          , 2,3,3,3      -- 2s
          , 3,4,4,4      -- 3s
          , 4,5,5,5      -- 4s
          , 5,6,6,6      -- 5s
          , 6,7,7,7      -- 6s
          , 7,8,8,8      -- 7s
          , 8,9,9,9      -- 8s
          , 9,10,10,10   -- 9s
          , 10,10,10,10  -- 10s
          , 10,10,10,10  -- 11s
          , 10,10        -- 12s
          ]

delay_t0 = testSF1 (delay 0.0 undefined)
delay_t0r =
  [ 0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0
  , 15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0
  ]

delay_t1 = testSF1 (delay 0.0001 17)
delay_t1r =
  [ 17.0,0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0
  , 15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0
  ]

delay_t2 = testSF2 (delay 0.0001 17)
delay_t2r =
  [ 17.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,2.0
  , 3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0
  ]

delay_t3 = testSF1 (time
                    >>> arr (\t -> sin (0.5 * t * pi + pi))
                    >>> loop (arr (\(x1,x2) -> let x' = max x1 x2 in (x',x'))
                              >>> second (delay 0.0001 0.0)))
delay_t3r =
  take 25
       (let xs = [ sin (0.5 * t * pi + pi) | t <- [0.0, 0.25 ..] ]
        in tail (scanl max 0 xs))

dts_t4 = take 15 (repeat 0.1)
         ++ [0.5, 0.5]
         ++ take 15 (repeat 0.1)
         ++ [2.0]
         ++ take 20 (repeat 0.1)

input_t4 = (0, [ (dt, Just i) | (dt, i) <- zip dts_t4 [1..] ])

delay_t4, delay_t4r :: [Int]
delay_t4 = take 100 (embed (delay 1.05 (-1)) input_t4)
delay_t4r =
  [ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 -- 0.0 s -- 0.9 s
  , -1,  0,  1,  2,  3,  4                 -- 1.0 s -- 1.5 s
  ,  9,                 14, 15, 15, 15, 15 -- 2.0 s -- 2.9 s
  , 15, 16, 16, 16, 16, 16, 17, 18, 19, 20 -- 3.0 s -- 3.9 s
  , 21                                     -- 4.0 s
  , 32, 32, 32, 32, 32, 32, 32, 32, 32, 32 -- 6.0 s -- 6.9 s
  , 32, 33, 34, 35, 36, 37, 38, 39, 40, 41 -- 7.0 s -- 7.9 s
  , 42                                     -- 8.0 s
  ]

delay_t5 = take 100 (drop 6 (embed sf (deltaEncode 0.1 (repeat ()))))
  where
    sf = time >>> arr (\t -> sin (2*pi*t)) >>> delay 0.55 (-1.0)

delay_t5r = take 100 (drop 6 (embed sf (deltaEncode 0.1 (repeat ()))))
  where
    sf = time >>> arr (\t -> sin (2*pi*(t-0.6)))

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
