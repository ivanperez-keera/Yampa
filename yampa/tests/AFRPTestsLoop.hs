{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsLoop.hs,v 1.6 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsLoop					     *
*       Purpose:        Test cases for loop				     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsLoop (loop_trs, loop_tr, loop_st0, loop_st0r, 
		      loop_st1, loop_st1r) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for loop
------------------------------------------------------------------------------

loop_acc :: SF (Double, Double) (Double, Double)
loop_acc = arr (\(x, y)->(x+y, x+y))

loop_t0 :: [Double]
loop_t0 = testSF1 (loop (constant (42.0, 43.0)))
loop_t0r = 
    [42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0,
     42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0,
     42.0, 42.0, 42.0, 42.0, 42.0]

loop_t1 :: [Double]
loop_t1 = testSF1 (loop identity)
loop_t1r =
    [0.0,  1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0,
     10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0,
     20.0, 21.0, 22.0, 23.0, 24.0]

loop_t2 :: [Time]
loop_t2 = testSF1 (loop (first localTime))
loop_t2r =
    [0.0,  0.25, 0.5,  0.75, 1.0,
     1.25, 1.5,  1.75, 2.0,  2.25,
     2.5,  2.75, 3.0,  3.25, 3.5,
     3.75, 4.0,  4.25, 4.5,  4.75,
     5.0,  5.25, 5.5,  5.75, 6.0]

-- AC, 10-March-2002: I think this is the simplest test that will
-- fail with AltST.
loop_t3 :: [Time]
loop_t3 = testSF1 (loop (second (iPre 0)))
loop_t3r =
    [0.0,  1.0,  2.0,  3.0,  4.0,
     5.0,  6.0,  7.0,  8.0,  9.0,
     10.0, 11.0, 12.0, 13.0, 14.0,
     15.0, 16.0, 17.0, 18.0, 19.0,
     20.0, 21.0, 22.0, 23.0, 24.0]

loop_t4 :: [Double]
loop_t4 = testSF1 (loop (second (iPre 0) >>> loop_acc))
loop_t4r =
    [0.0,   1.0,   3.0,   6.0,   10.0, 
     15.0,  21.0,  28.0,  36.0,  45.0,
     55.0,  66.0,  78.0,  91.0,  105.0,
     120.0, 136.0, 153.0, 171.0, 190.0,
     210.0, 231.0, 253.0, 276.0, 300.0]

loop_t5 :: [Double]
loop_t5 = testSF2 (loop (second (iPre 0) >>> loop_acc))
loop_t5r =
    [0.0,  0.0,  0.0,  0.0,  0.0, 
     1.0,  2.0,  3.0,  4.0,  5.0,
     7.0,  9.0,  11.0, 13.0, 15.0,
     18.0, 21.0, 24.0, 27.0, 30.0,
     34.0, 38.0, 42.0, 46.0, 50.0]

loop_t6 :: [Double]
loop_t6 = testSF1 (loop (iPre (0,0) >>> first localTime >>> loop_acc))
loop_t6r =
    [0.0,   0.25,  0.75,  1.5,   2.5,
     3.75,  5.25,  7.0,   9.0,   11.25,
     13.75, 16.5,  19.5,  22.75, 26.25,
     30.0,  34.0,  38.25, 42.75, 47.5,
     52.5,  57.75, 63.25, 69.0,  75.0]

loop_t7 :: [Double]
loop_t7 = testSF1 (loop (loop_acc >>> second (iPre 0)))
loop_t7r = loop_t4r

loop_t8 :: [Double]
loop_t8 = testSF2 (loop (loop_acc >>> second (iPre 0)))
loop_t8r = loop_t5r

loop_t9 :: [Double]
loop_t9 = testSF1 (loop (first localTime >>> loop_acc >>> iPre (0,0)))
loop_t9r =
    [0.0,   0.0,   0.25,  0.75,  1.5,
     2.5,   3.75,  5.25,  7.0,   9.0,
     11.25, 13.75, 16.5,  19.5,  22.75,
     26.25, 30.0,  34.0,  38.25, 42.75,
     47.5,  52.5,  57.75, 63.25, 69.0]

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
    let
	es = embed (loop (second integral >>> arr (\(_, x) -> (x + 1, x + 1))))
                   (deltaEncode 0.001 (repeat ()))
    in
	[es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]
loop_t13r = [1.0,2.71692, 7.38167, 20.05544, 54.48911, 148.04276]

loop_t14 :: [Double]
loop_t14 =
    let
	es = embed (loop (arr (\(_, x) -> (x + 1, x + 1)) >>> second integral))
                   (deltaEncode 0.001 (repeat ()))
    in
	[es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]
loop_t14r = loop_t13r

loop_t15 :: [Double]
loop_t15 =
    let
	es = embed (loop (arr (\(_, x) -> (x + 1, x + 1))
                          >>> second integral
			  >>> identity))
                   (deltaEncode 0.001 (repeat ()))
    in
	[es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]
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
  [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,
   16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0]

loop_trs =
    [ loop_t0  ~= loop_t0r,
      loop_t1  ~= loop_t1r,
      loop_t2  ~= loop_t2r,
      loop_t3  ~= loop_t3r,
      loop_t4  ~= loop_t4r,
      loop_t5  ~= loop_t5r,
      loop_t6  ~= loop_t6r,
      loop_t7  ~= loop_t7r,
      loop_t8  ~= loop_t8r,
      loop_t9  ~= loop_t9r,
      loop_t10 ~= loop_t10r,
      loop_t11 ~= loop_t11r,
      loop_t12 ~= loop_t12r,
      loop_t13 ~= loop_t13r,
      loop_t14 ~= loop_t14r,
      loop_t15 ~= loop_t15r,
      loop_t16 ~= loop_t16r,
      loop_t17 ~= loop_t17r
    ]

loop_tr = and loop_trs

loop_st0 = testSFSpaceLeak 2000000
			   (loop (second (iPre 0) >>> loop_acc))
loop_st0r = 9.999995e11

-- A simple loop test taken from MiniYampa:
-- This results in pulling on the fed-back output during evaluation, because
-- switch is strict in its input sample:
loop_st1 :: Double
loop_st1 = testSFSpaceLeak 2000000
             (loop $ second $ (switch identity (const (arr fst))) >>> arr (\x -> (x + x + x + x + x + x + x,noEvent)) >>> (iPre (25, noEvent)))
loop_st1r = 999999.5