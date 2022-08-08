{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP    #-}
-- |
-- Description : Test cases for FRP.Yampa.Conditional
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.Conditional
    ( tests
    )
  where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<*>))
import Data.Functor        ((<$>))
#endif
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa
import FRP.Yampa.Conditional (pause, provided)
import FRP.Yampa.LTLFuture   (TPred (Always, SP), evalT)
import FRP.Yampa.QuickCheck  (uniDistStream)
import FRP.Yampa.Stream      (SignalSampleStream)

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Conditional"
  [ testProperty "provided (1, fixed)" (property $ utils_t8 ~= utils_t8r)
  , testProperty "provided (2, fixed)" (property $ utils_t9 ~= utils_t9r)
  , testProperty "pause (qc)"          propPause
  ]

-- * Guards and automata-oriented combinators

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

propPause :: Property
propPause =
    forAll initialValueG $ \initialValue ->
    forAll myStream $ evalT $
      -- The behavior of pause is always the same as some ideal behavior
      -- implemented by modelPause below. We give these auxiliary definitions
      -- names and pass initialValue as argument to facilitate debugging.
      Always $ SP $ (==) <$> sfPause initialValue <*> sfModelPause initialValue
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    initialValueG :: Gen Float
    initialValueG = arbitrary

    -- SF that uses the actual function being tested
    sfPause :: Float -> SF Float Float
    sfPause initialValue = pause initialValue (arr (odd . round)) integral

    -- Model SF that uses the actual function being tested
    sfModelPause :: Float -> SF Float Float
    sfModelPause initialValue =
      modelPause initialValue (arr (odd . round)) integral

    -- Model implementation of pause.
    modelPause :: b -> SF a Bool -> SF a b -> SF a b
    modelPause acc0 sf1 sf2 = proc (a) -> do
      rec c <- sf1 -< a

          -- Accumulator that is updated only when then condition is false.
          acc <- hold acc0 -< e

          -- When the condition is false, sf2 is turned on and executed,
          -- producing a new Event. Note that we need to put this in an
          -- ArrowCase block, we can't just run both and the decide whether we
          -- want to output the value or not based on the condition, because,
          -- in that case, the argument sf2 would still be executed and
          -- accumulate state.
          e <- if c then returnA -< NoEvent
                    else Event ^<< sf2 -< a

      returnA -< acc
