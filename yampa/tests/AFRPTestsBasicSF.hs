{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsBasicSF.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsBasicSF				     *
*       Purpose:        Test cases for basic signal functions		     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsBasicSF (basicsf_trs, basicsf_tr) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for basic signal functions and initialization
------------------------------------------------------------------------------

basicsf_t0 :: [Double]
basicsf_t0 = testSF1 identity
basicsf_t0r =
    [0.0,  1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0,
     10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0,
     20.0, 21.0, 22.0, 23.0, 24.0]


basicsf_t1 :: [Double]
basicsf_t1 = testSF1 (constant 42.0)
basicsf_t1r =
    [42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0,
     42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0,
     42.0, 42.0, 42.0, 42.0, 42.0]

basicsf_t2 :: [Double]
basicsf_t2 = testSF1 localTime
basicsf_t2r =
    [0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25,
     2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0, 4.25, 4.5, 4.75,
     5.0, 5.25, 5.5, 5.75, 6.0]

basicsf_t3 :: [Double]
basicsf_t3 = testSF1 time
basicsf_t3r =
    [0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25,
     2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0, 4.25, 4.5, 4.75,
     5.0, 5.25, 5.5, 5.75, 6.0]

basicsf_t4 :: [Double]
basicsf_t4 = testSF1 (initially 42.0)
basicsf_t4r =
    [42.0, 1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0,
     10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0,
     20.0, 21.0, 22.0, 23.0, 24.0]


basicsf_trs =
    [ basicsf_t0 ~= basicsf_t0r,
      basicsf_t1 ~= basicsf_t1r,
      basicsf_t2 ~= basicsf_t2r,
      basicsf_t3 ~= basicsf_t3r,
      basicsf_t4 ~= basicsf_t4r
    ]

basicsf_tr = and basicsf_trs
