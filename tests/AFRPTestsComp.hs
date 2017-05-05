{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsComp.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsComp					     *
*       Purpose:        Test cases for (>>>)				     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsComp (comp_tr, comp_trs) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for comp
------------------------------------------------------------------------------

comp_t0 = testSF1 ((arr (+1)) >>> (arr (+2)))
comp_t0r :: [Double]
comp_t0r =
    [3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,
     18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0]

comp_t1 = testSF2 ((arr (+1)) >>> (arr (+2)))
comp_t1r :: [Double]
comp_t1r =
    [3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,5.0,
     6.0,6.0,6.0,6.0,6.0,7.0,7.0,7.0,7.0,7.0]

comp_t2 = testSF1 ((constant 5.0) >>> (arr (+1)))
comp_t2r :: [Double]
comp_t2r =
    [6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,
     6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0]

comp_t3 = testSF2 ((constant 5.0) >>> (arr (+1)))
comp_t3r :: [Double]
comp_t3r =
    [6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,
     6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0]

-- Integration by the rectangle rule or trapezoid rule makes no difference.
comp_t4 = testSF1 ((constant 2.0) >>> integral)
comp_t4r :: [Double]
comp_t4r =
    [0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,
     9.0,9.5,10.0,10.5,11.0,11.5,12.0]

-- Same result as above.
comp_t5 = testSF2 ((constant 2.0) >>> integral)
comp_t5r :: [Double]
comp_t5r =
    [0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,
     9.0,9.5,10.0,10.5,11.0,11.5,12.0]

comp_trs =
    [ comp_t0 ~= comp_t0r,
      comp_t1 ~= comp_t1r,
      comp_t2 ~= comp_t2r,
      comp_t3 ~= comp_t3r,
      comp_t4 ~= comp_t4r,
      comp_t5 ~= comp_t5r
    ]

comp_tr = and comp_trs
