{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsDer.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsDer					     *
*       Purpose:        Test cases for derivative			     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsDer (der_tr, der_trs) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for derivative
------------------------------------------------------------------------------

der_step = 0.001
der_N = 1000

der_t0 :: [Double]
der_t0 = take der_N $	-- First value is always 0
         embed derivative
               (deltaEncode der_step
			    [sin(2 * pi * t) | t <- [0.0, der_step ..]])
{-
-- For stepsize 0.1
der_t0r :: [Double]
der_t0r =
    [ 0.0000,  5.8779,  3.6327, 0.0000, -3.6327,
     -5.8779, -5.8779, -3.6327, 0.0000,  3.6327,
      5.8779,  5.8779,  3.6327, 0.0000, -3.6327,
     -5.8779, -5.8779, -3.6327, 0.0000,  3.6327]
-}

der_t0r :: [Double]
der_t0r = take der_N $ 
          [2 * pi * cos (2 * pi * t) | t <- [0.0, der_step ..]]

-- We're happy if we are in the right ball park.
der_t0_max_diff = (maximum (zipWith (\x y -> abs (x - y))
                                    (tail der_t0)
                                    (tail der_t0r)))

der_trs =
    [ der_t0_max_diff < 0.05
    ]

der_tr = and der_trs
