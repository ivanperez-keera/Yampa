-- |
-- Description : Test cases for SFs with loops
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.Loop
    ( tests
    )
  where

import Data.Tuple (swap)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Loop"
  [ testProperty "loop (0, fixed)"         (property $ loop_t0  ~= loop_t0r)
  , testProperty "loop (1, fixed)"         (property $ loop_t1  ~= loop_t1r)
  , testProperty "loop (2, fixed)"         (property $ loop_t2  ~= loop_t2r)
  , testProperty "loop (3, fixed)"         (property $ loop_t3  ~= loop_t3r)
  , testProperty "loop (4, fixed)"         (property $ loop_t4  ~= loop_t4r)
  , testProperty "loop (5, fixed)"         (property $ loop_t5  ~= loop_t5r)
  , testProperty "loop (6, fixed)"         (property $ loop_t6  ~= loop_t6r)
  , testProperty "loop (7, fixed)"         (property $ loop_t7  ~= loop_t7r)
  , testProperty "loop (8, fixed)"         (property $ loop_t8  ~= loop_t8r)
  , testProperty "loop (9, fixed)"         (property $ loop_t9  ~= loop_t9r)
  , testProperty "loop (10, fixed)"        (property $ loop_t10 ~= loop_t10r)
  , testProperty "loop (11, fixed)"        (property $ loop_t11 ~= loop_t11r)
  , testProperty "loop (12, fixed)"        (property $ loop_t12 ~= loop_t12r)
  , testProperty "loop (13, fixed)"        (property $ loop_t13 ~= loop_t13r)
  , testProperty "loop (14, fixed)"        (property $ loop_t14 ~= loop_t14r)
  , testProperty "loop (15, fixed)"        (property $ loop_t15 ~= loop_t15r)
  , testProperty "loop (16, fixed)"        (property $ loop_t16 ~= loop_t16r)
  , testProperty "loop (17, fixed)"        (property $ loop_t17 ~= loop_t17r)
  , testProperty "loop laws (0, fixed)"    (property $ looplaws_t0_lhs  ~= looplaws_t0_rhs)
  , testProperty "loop laws (1, fixed)"    (property $ looplaws_t1_lhs  ~= looplaws_t1_rhs)
  , testProperty "loop laws (2, fixed)"    (property $ looplaws_t2_lhs  ~= looplaws_t2_rhs)
  , testProperty "loop laws (3, fixed)"    (property $ looplaws_t3_lhs  ~= looplaws_t3_rhs)
  , testProperty "loop laws (4, fixed)"    (property $ looplaws_t4_lhs  ~= looplaws_t4_rhs)
  , testProperty "loop laws (5, fixed)"    (property $ looplaws_t5_lhs  ~= looplaws_t5_rhs)
  , testProperty "loopIntegral (0, fixed)" (property $ loopIntegral_t0 ~= loopIntegral_t0r)
  , testProperty "loopIntegral (1, fixed)" (property $ loopIntegral_t1 ~= loopIntegral_t1r)
  ]

-- * Test cases for loop

loop_acc :: SF (Double, Double) (Double, Double)
loop_acc = arr (\(x, y)->(x+y, x+y))

loop_t0 :: [Double]
loop_t0 = testSF1 (loop (constant (42.0, 43.0)))
loop_t0r =
  [ 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0
  , 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0
  , 42.0, 42.0, 42.0, 42.0, 42.0
  ]

loop_t1 :: [Double]
loop_t1 = testSF1 (loop identity)
loop_t1r =
  [ 0.0,  1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0
  , 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0
  , 20.0, 21.0, 22.0, 23.0, 24.0
  ]

loop_t2 :: [Time]
loop_t2 = testSF1 (loop (first localTime))
loop_t2r =
  [ 0.0,  0.25, 0.5,  0.75, 1.0
  , 1.25, 1.5,  1.75, 2.0,  2.25
  , 2.5,  2.75, 3.0,  3.25, 3.5
  , 3.75, 4.0,  4.25, 4.5,  4.75
  , 5.0,  5.25, 5.5,  5.75, 6.0
  ]

-- AC, 10-March-2002: I think this is the simplest test that will
-- fail with AltST.
loop_t3 :: [Time]
loop_t3 = testSF1 (loop (second (iPre 0)))
loop_t3r =
  [ 0.0,  1.0,  2.0,  3.0,  4.0
  , 5.0,  6.0,  7.0,  8.0,  9.0
  , 10.0, 11.0, 12.0, 13.0, 14.0
  , 15.0, 16.0, 17.0, 18.0, 19.0
  , 20.0, 21.0, 22.0, 23.0, 24.0
  ]

loop_t4 :: [Double]
loop_t4 = testSF1 (loop (second (iPre 0) >>> loop_acc))
loop_t4r =
  [ 0.0,   1.0,   3.0,   6.0,   10.0
  , 15.0,  21.0,  28.0,  36.0,  45.0
  , 55.0,  66.0,  78.0,  91.0,  105.0
  , 120.0, 136.0, 153.0, 171.0, 190.0
  , 210.0, 231.0, 253.0, 276.0, 300.0
  ]

loop_t5 :: [Double]
loop_t5 = testSF2 (loop (second (iPre 0) >>> loop_acc))
loop_t5r =
  [ 0.0,  0.0,  0.0,  0.0,  0.0
  , 1.0,  2.0,  3.0,  4.0,  5.0
  , 7.0,  9.0,  11.0, 13.0, 15.0
  , 18.0, 21.0, 24.0, 27.0, 30.0
  , 34.0, 38.0, 42.0, 46.0, 50.0
  ]

loop_t6 :: [Double]
loop_t6 = testSF1 (loop (iPre (0,0) >>> first localTime >>> loop_acc))
loop_t6r =
  [ 0.0,   0.25,  0.75,  1.5,   2.5
  , 3.75,  5.25,  7.0,   9.0,   11.25
  , 13.75, 16.5,  19.5,  22.75, 26.25
  , 30.0,  34.0,  38.25, 42.75, 47.5
  , 52.5,  57.75, 63.25, 69.0,  75.0
  ]

loop_t7 :: [Double]
loop_t7 = testSF1 (loop (loop_acc >>> second (iPre 0)))
loop_t7r = loop_t4r

loop_t8 :: [Double]
loop_t8 = testSF2 (loop (loop_acc >>> second (iPre 0)))
loop_t8r = loop_t5r

loop_t9 :: [Double]
loop_t9 = testSF1 (loop (first localTime >>> loop_acc >>> iPre (0,0)))
loop_t9r =
  [ 0.0,   0.0,   0.25,  0.75,  1.5
  , 2.5,   3.75,  5.25,  7.0,   9.0
  , 11.25, 13.75, 16.5,  19.5,  22.75
  , 26.25, 30.0,  34.0,  38.25, 42.75
  , 47.5,  52.5,  57.75, 63.25, 69.0
  ]

loop_t10 :: [Double]
loop_t10 = testSF1 (loop (loop_acc >>> second (iPre 0) >>> identity))
loop_t10r = loop_t4r

loop_t11 :: [Double]
loop_t11 = testSF2 (loop (loop_acc >>> second (iPre 0) >>> identity))
loop_t11r = loop_t5r

loop_t12 :: [Double]
loop_t12 = testSF1 (loop (first localTime
                          >>> loop_acc
                          >>> iPre (0,0)
                          >>> identity))
loop_t12r = loop_t9r

-- Computation of approximation to exp 0, exp 1, ..., exp 5 by integration.
-- Values as given by using exp directly:
-- 1.0, 2.71828, 7.38906, 20.0855, 54.5981, 148.413
loop_t13 :: [Double]
loop_t13 =
  let es = embed (loop (second integral >>> arr (\(_, x) -> (x + 1, x + 1))))
                 (deltaEncode 0.001 (repeat ()))
  in [es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]

loop_t13r = [1.0,2.71692, 7.38167, 20.05544, 54.48911, 148.04276]

loop_t14 :: [Double]
loop_t14 =
  let es = embed (loop (arr (\(_, x) -> (x + 1, x + 1)) >>> second integral))
                 (deltaEncode 0.001 (repeat ()))
  in [es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]
loop_t14r = loop_t13r

loop_t15 :: [Double]
loop_t15 =
  let es = embed (loop (arr (\(_, x) -> (x + 1, x + 1))
                        >>> second integral
                        >>> identity))
                 (deltaEncode 0.001 (repeat ()))
  in [es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]
loop_t15r = loop_t13r

-- A generator for factorial:  The least-fixed point of this function is
-- the factorial function.

factGen f n = if (n==0) then 1 else n*f(n-1)

-- Can we use loop to construct a fixed point?
loop_t16 :: [Int]
loop_t16 = testSF1 (loop $ arr (\ (_,f) -> (f 4,factGen f)))
loop_t16r =
  [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24]

-- A simple loop test taken from MiniYampa:
-- This results in pulling on the fed-back output during evaluation, because
-- switch is strict in its input sample:
loop_t17 :: [Double]
loop_t17 = testSF1 (loop $ second $ (switch identity (const (arr fst))) >>> arr (\x -> (x,noEvent)) >>> (iPre (25, noEvent)))
loop_t17r =
  [ 0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0
  , 16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0
  ]

-- * Test cases based on the arrow laws for loop

-- For a description of the laws, see Ross Paterson: Embedding a Class of
-- Domain-Specific Languages in a Functional Language.
-- Only a very rudimentary sanity check. Obviously not intended to "prove"
-- this implementation indeed do respect the laws.

simple_loop :: ((a,c) -> (b,c)) -> (a -> b)
simple_loop f a = b
  where
    (b, c) = f (a, c)

-- Left tightening
looplaws_t0_f = second integral >>> arr swap
looplaws_t0_h :: Fractional a => SF a a
looplaws_t0_h = arr (+10.0)
looplaws_t0_lhs :: [Double]
looplaws_t0_lhs = testSF1 (loop (first looplaws_t0_h >>> looplaws_t0_f))
looplaws_t0_rhs :: [Double]
looplaws_t0_rhs = testSF1 (looplaws_t0_h >>> loop looplaws_t0_f)

-- Right tightening
looplaws_t1_f = second integral >>> arr swap
looplaws_t1_h :: Fractional a => SF a a
looplaws_t1_h = arr (+10.0)
looplaws_t1_lhs :: [Double]
looplaws_t1_lhs = testSF1 (loop (looplaws_t1_f >>> first looplaws_t1_h))
looplaws_t1_rhs :: [Double]
looplaws_t1_rhs = testSF1 (loop looplaws_t1_f >>> looplaws_t1_h)

-- Sliding
-- Used to work with only signature t2_f :: Fractional a -> SF a a
looplaws_t2_f :: SF (Double, Double) (Double, Double)
looplaws_t2_f = integral
looplaws_t2_k = id *** (+42.0)
looplaws_t2_lhs :: [Double]
looplaws_t2_lhs = testSF1 (loop (looplaws_t2_f >>> arr looplaws_t2_k))
looplaws_t2_rhs :: [Double]
looplaws_t2_rhs = testSF1 (loop (arr looplaws_t2_k >>> looplaws_t2_f))

-- Vanishing
-- The lazy pattern matching (~) is necessary to avoid a black hole in the
-- RHS due to premature forcing of tuples. As far as I can tell, loop is
-- as lazy as it can be, and this problem could not have been solved by
-- "fixing" the loop definition.
looplaws_t3_f = second integral
                >>> first (arr swap)
                >>> arr (\ ~((a,b),c) -> ((a,c),b))
looplaws_t3_lhs :: [Double]
looplaws_t3_lhs = testSF1 (loop (loop looplaws_t3_f))
looplaws_t3_rhs :: [Double]
looplaws_t3_rhs = testSF1 (loop (arr assocInv >>> looplaws_t3_f >>> arr assoc))

-- Superposing
looplaws_t4_f = second integral >>> arr swap
looplaws_t4_lhs :: [(Double,Double)]
looplaws_t4_lhs = testSF1 (arr dup >>> (second (loop looplaws_t4_f)))
looplaws_t4_rhs :: [(Double, Double)]
looplaws_t4_rhs = testSF1 (arr dup >>> (loop (arr assoc
                                        >>> second looplaws_t4_f
                                        >>> arr assocInv)))

-- Extension
looplaws_t5_f = \(a,c) -> (take 5 c, a : c)
looplaws_t5_lhs :: [[Double]]
looplaws_t5_lhs = testSF1 (loop (arr looplaws_t5_f))
looplaws_t5_rhs :: [[Double]]
looplaws_t5_rhs = testSF1 (arr (simple_loop looplaws_t5_f))

-- * Test cases for loopIntegral

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
