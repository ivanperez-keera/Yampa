-- |
-- Description : Test cases for SFs with loops
-- Copyright   : (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- Authors     : Antony Courtney, Henrik Nilsson
module Test.FRP.Yampa.Loop
    ( tests
    )
  where

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Loop"
  [ testProperty "loopPre (0, fixed)"      (property $ loopPre_t0 ~= loopPre_t0r)
  , testProperty "loopPre (1, fixed)"      (property $ loopPre_t1 ~= loopPre_t1r)
  , testProperty "loopPre (2, fixed)"      (property $ loopPre_t2 ~= loopPre_t2r)
  , testProperty "loopPre (3, fixed)"      (property $ loopPre_t3 ~= loopPre_t3r)
  , testProperty "loopPre (4, fixed)"      (property $ loopPre_t4 ~= loopPre_t4r)
  , testProperty "loopIntegral (0, fixed)" (property $ loopIntegral_t0 ~= loopIntegral_t0r)
  , testProperty "loopIntegral (1, fixed)" (property $ loopIntegral_t1 ~= loopIntegral_t1r)
  ]

-- * Loops with guaranteed well-defined feedback

loop_acc :: SF (Double, Double) (Double, Double)
loop_acc = arr (\(x, y)->(x+y, x+y))

-- This kind of test will fail for infinitesimal delay!
loopPre_t0 = testSF1 (loopPre 0 loop_acc)
loopPre_t0r =
  [ 0.0,1.0,3.0,6.0,10.0,15.0,21.0,28.0,36.0,45.0,55.0,66.0,78.0,91.0
  , 105.0,120.0,136.0,153.0,171.0,190.0,210.0,231.0,253.0,276.0,300.0
  ]

loopPre_t1 = testSF2 (loopPre 0 loop_acc)
loopPre_t1r =
  [ 0.0,0.0,0.0,0.0,0.0,1.0,2.0,3.0,4.0,5.0,7.0,9.0,11.0,13.0,15.0,18.0
  , 21.0,24.0,27.0,30.0,34.0,38.0,42.0,46.0,50.0
  ]

-- This kind of test will fail for infinitesimal delay!
loopPre_t2 = testSF1 (loopPre False (arr (dup . not . snd)))
loopPre_t2r =
  [ True,False,True,False,True,False,True,False,True,False,True,False
  , True,False,True,False,True,False,True,False,True,False,True,False,True
  ]

loopPre_t3 = testSF1 (loopPre 0 (first localTime))
loopPre_t3r =
  [ 0.0,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75
  , 4.0,4.25,4.5,4.75,5.0,5.25,5.5,5.75,6.0
  ]

loopPre_t4 = testSF1 (loopPre 0 (first localTime >>> loop_acc))
loopPre_t4r =
  [ 0.0,0.25,0.75,1.5,2.5,3.75,5.25,7.0,9.0,11.25,13.75,16.5,19.5,22.75
  , 26.25,30.0,34.0,38.25,42.75,47.5,52.5,57.75,63.25,69.0,75.0
  ]

-- Computation of approximation to exp 0, exp 1, ..., exp 5 by integration.
-- Values as given by using exp directly:
-- 1.0, 2.71828, 7.38906, 20.0855, 54.5981, 148.413
loopIntegral_t0 =
  let es = embed (loopIntegral (arr (\(_, x) -> (x + 1, x + 1))))
                 (deltaEncode 0.001 (repeat ()))
  in [es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]
loopIntegral_t0r :: [Double]
loopIntegral_t0r = [1.0,2.71692,7.38167,20.05544,54.48911,148.04276]

-- Test case with a time varying signal transformer inside the loop.
-- Starting at position 0 [m], accelerate by 1.0 [m/s^2] until position
-- exceeds 2.0 [m]. Then accelerate by -1.0 [m/s^2] until position gets
-- below 0.0 [m]. Then accelerate at 1.0 [m/s^2] again. And so on.

type Position = Double
type Velocity = Double
type Acceleration = Double

posCntrl :: SF b Position
posCntrl = loopIntegral posCntrlNR
  where
    posCntrlNR :: SF (b, Velocity) (Position, Acceleration)
    posCntrlNR =
      arr snd                     -- Get the velocity.
      >>> integral                -- This integral gives us the position.
      >>> arr (\x -> (x,x))
      >>>
          (second $
             arr (\x -> (x,x))
             >>>
                 (first $
                    arr (>=2.0)
                    >>> edge
                    >>> (arr (fmap (const (constant (-1.0))))))
             >>>
                 (second $
                    arr (< 0.0)
                    >>> edge
                    >>> (arr (fmap (const (constant 1.0)))))
             >>> arr (\(e1,e2) -> e1 `lMerge` e2)
             >>> arr (\e -> ((), e))
             >>> rSwitch (constant 1.0))

loopIntegral_t1 = take 250 (embed posCntrl (deltaEncode 0.1 (repeat ())))

-- Result only partially verified. But the sign of the acceleration changes
-- at roughly the right points.
loopIntegral_t1r :: [Double]
loopIntegral_t1r =
  [ 0.0,0.0,0.01,0.03,0.06,0.1,0.15,0.21,0.28,0.36,0.45,0.55,0.66,0.78,0.91
  , 1.05,1.2,1.36,1.53,1.71,1.9,2.1,2.31,2.51,2.7,2.88,3.05,3.21,3.36,3.5
  , 3.63,3.75,3.86,3.96,4.05,4.13,4.2,4.26,4.31,4.35,4.38,4.4,4.41,4.41,4.4
  , 4.38,4.35,4.31,4.26,4.2,4.13,4.05,3.96,3.86,3.75,3.63,3.5,3.36,3.21,3.05
  , 2.88,2.7,2.51,2.31,2.1,1.88,1.65,1.41,1.16,0.9,0.63,0.35,0.06,-0.24
  , -0.55,-0.85,-1.14,-1.42,-1.69,-1.95,-2.2,-2.44,-2.67,-2.89,-3.1,-3.3
  , -3.49,-3.67,-3.84,-4.0,-4.15,-4.29,-4.42,-4.54,-4.65,-4.75,-4.84,-4.92
  , -4.99,-5.05,-5.1,-5.14,-5.17,-5.19,-5.2,-5.2,-5.19,-5.17,-5.14,-5.1
  , -5.05,-4.99,-4.92,-4.84,-4.75,-4.65,-4.54,-4.42,-4.29,-4.15,-4.0,-3.84
  , -3.67,-3.49,-3.3,-3.1,-2.89,-2.67,-2.44,-2.2,-1.95,-1.69,-1.42,-1.14
  , -0.85,-0.55,-0.24,0.08,0.41,0.75,1.1,1.46,1.83,2.21,2.6,2.98,3.35,3.71
  , 4.06,4.4,4.73,5.05,5.36,5.66,5.95,6.23,6.5,6.76,7.01,7.25,7.48,7.7,7.91
  , 8.11,8.3,8.48,8.65,8.81,8.96,9.1,9.23,9.35,9.46,9.56,9.65,9.73,9.8,9.86
  , 9.91,9.95,9.98,10.0,10.01,10.01,10.0,9.98,9.95,9.91,9.86,9.8,9.73,9.65
  , 9.56,9.46,9.35,9.23,9.1,8.96,8.81,8.65,8.48,8.3,8.11,7.91,7.7,7.48,7.25
  , 7.01,6.76,6.5,6.23,5.95,5.66,5.36,5.05,4.73,4.4,4.06,3.71,3.35,2.98,2.6
  , 2.21,1.81,1.4,0.98,0.55,0.11,-0.34,-0.80,-1.25,-1.69,-2.12,-2.54,-2.95
  , -3.35,-3.74,-4.12,-4.49,-4.85,-5.2,-5.54,-5.87,-6.19,-6.5,-6.8,-7.09
  , -7.37,-7.64,-7.9
  ]
