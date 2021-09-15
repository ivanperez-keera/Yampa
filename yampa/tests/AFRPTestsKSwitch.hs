{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsKSwitch.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsKSwitch				     *
*       Purpose:        Test cases for kSwitch and dkSwitch		     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsKSwitch (kswitch_tr, kswitch_trs) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for kSwitch and dkSwitch
------------------------------------------------------------------------------

kswitch_inp1 = deltaEncode 0.1 [0.0, 0.5 ..]

whenSndGE :: Ord b => b -> c -> SF (a, b) (Event c)
whenSndGE b c = arr snd >>> arr (>= b) >>> edge >>> arr (`tag` c)


kswitch_t0 :: [Double]
kswitch_t0 = take 20 $ embed sf kswitch_inp1
    where
	sf =
	    kSwitch integral (whenSndGE 0.2 (-1.0)) $ \sf1 x ->
	    kSwitch (integral >>> arr (+x)) (whenSndGE 1.0 (1.0)) $ \_ _ ->
            sf1

kswitch_t0r =
    [ 0.00,  0.00,  0.05, 0.15, -1.00,
     -0.80, -0.55, -0.25, 0.10,  0.50,
      0.95,  0.30,  0.85, 1.45,  2.10,
      2.80,  3.55,  4.35, 5.20,  6.10]


kswitch_t1 :: [Double]
kswitch_t1 = take 20 $ embed sf kswitch_inp1
    where
	sf =
	    dkSwitch integral (whenSndGE 0.2 (-1.0)) $ \sf1 x ->
	    dkSwitch (integral >>> arr (+x)) (whenSndGE 1.0 (1.0)) $ \_ _ ->
            sf1

kswitch_t1r =
    [ 0.00,  0.00,  0.05, 0.15, 0.30,
     -0.80, -0.55, -0.25, 0.10, 0.50,
      0.95,  1.45,  0.85, 1.45, 2.10,
      2.80,  3.55,  4.35, 5.20, 6.10]


kswitch_t2 :: [Double]
kswitch_t2 = take 20 $ embed sf kswitch_inp1
    where
	sf =
	    kSwitch integral (now (-1.0)) $ \sf1 x ->
	    kSwitch (integral >>> arr (+x)) (whenSndGE 1.0 (1.0)) $ \_ _ ->
            sf1

kswitch_t2r =
    [-1.00, -1.00, -0.95, -0.85, -0.70,
     -0.50, -0.25,  0.05,  0.40,  0.80,
      0.00,  0.50,  1.05,  1.65,  2.30,
      3.00,  3.75,  4.55,  5.40,  6.30]


kswitch_t3 :: [Double]
kswitch_t3 = take 20 $ embed sf kswitch_inp1
    where
	sf =
	    dkSwitch integral (now (-1.0)) $ \sf1 x ->
	    dkSwitch (integral >>> arr (+x)) (whenSndGE 1.0 (1.0)) $ \_ _ ->
            sf1

kswitch_t3r =
    [ 0.00, -1.00, -0.95, -0.85, -0.70,
     -0.50, -0.25,  0.05,  0.40,  0.80,
      1.25,  0.50,  1.05,  1.65,  2.30,
      3.00,  3.75,  4.55,  5.40,  6.30]


-- The correct strictness properties of dkSwitch are crucial here.
-- kSwitch does not work.
kswitch_t4 = take 40 $
    embed (loop $
	       dkSwitch sf (sfe 0.55 (-1.0))              $ \sf1 x ->
	       dkSwitch (sf >>> arr2 (+x)) (sfe 0.05 8.0) $ \sf2 y ->
	       dkSwitch sf1 (sfe 2.0 (-2.0))              $ \_   z ->
	       sf2 >>> arr2 (+(y + z))
           )
          (deltaEncode 0.1 (repeat ()))
    where
        sf :: SF (a, Double) (Double, Double)
        sf = constant 1.0 >>> integral >>> arr dup

	sfe :: Double -> Double -> SF ((a, Double), b) (Event Double)
	sfe x e = arr fst >>> whenSndGE x e

	arr2 f = arr (\(x,y) -> (f x, f y))

kswitch_t4r =
    [ 0.0,  0.1,  0.2,  0.3,  0.4,
      0.5,  0.6, -0.9, -0.8, -0.7,
     -0.6, -0.5, -0.4, -0.3, -0.2,
     -0.1,  0.0,  0.1,  0.7,  0.8,
      0.9,  1.0,  1.1,  1.2,  1.3,
      1.4,  1.5,  1.6,  1.7,  1.8,
      1.9,  2.0,  6.2,  6.3,  6.4,
      6.5,  6.6,  6.7,  6.8,  6.9]


kswitch_trs =
    [ kswitch_t0 ~= kswitch_t0r,
      kswitch_t1 ~= kswitch_t1r,
      kswitch_t2 ~= kswitch_t2r,
      kswitch_t3 ~= kswitch_t3r,
      kswitch_t4 ~= kswitch_t4r
    ]

kswitch_tr = and kswitch_trs
