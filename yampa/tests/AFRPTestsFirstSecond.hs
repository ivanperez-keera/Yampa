{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsFirstSecond.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsArr                                         *
*       Purpose:        Test cases for first and second			     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsFirstSecond (first_trs, first_tr, second_trs, second_tr) where

import Data.Tuple (swap)

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for first
------------------------------------------------------------------------------

first_t0 :: [(Int,Double)]
first_t0 = testSF1 (arr dup >>> first (constant 7))
first_t0r :: [(Int,Double)]
first_t0r =
    [(7,0.0),  (7,1.0),  (7,2.0),  (7,3.0),  (7,4.0),
     (7,5.0),  (7,6.0),  (7,7.0),  (7,8.0),  (7,9.0),
     (7,10.0), (7,11.0), (7,12.0), (7,13.0), (7,14.0),
     (7,15.0), (7,16.0), (7,17.0), (7,18.0), (7,19.0),
     (7,20.0), (7,21.0), (7,22.0), (7,23.0), (7,24.0)]

first_t1 :: [(Int,Double)]
first_t1 = testSF2 (arr dup >>> first (constant 7))
first_t1r :: [(Int,Double)]
first_t1r =
    [(7,0.0), (7,0.0), (7,0.0), (7,0.0), (7,0.0),
     (7,1.0), (7,1.0), (7,1.0), (7,1.0), (7,1.0),
     (7,2.0), (7,2.0), (7,2.0), (7,2.0), (7,2.0),
     (7,3.0), (7,3.0), (7,3.0), (7,3.0), (7,3.0),
     (7,4.0), (7,4.0), (7,4.0), (7,4.0), (7,4.0)]

first_t2 :: [(Double,Double)]
first_t2 = testSF1 (arr dup >>> first (arr (+1)))
first_t2r =
    [(1.0,0.0),   (2.0,1.0),   (3.0,2.0),   (4.0,3.0),   (5.0,4.0),
     (6.0,5.0),   (7.0,6.0),   (8.0,7.0),   (9.0,8.0),   (10.0,9.0),
     (11.0,10.0), (12.0,11.0), (13.0,12.0), (14.0,13.0), (15.0,14.0),
     (16.0,15.0), (17.0,16.0), (18.0,17.0), (19.0,18.0), (20.0,19.0),
     (21.0,20.0), (22.0,21.0), (23.0,22.0), (24.0,23.0), (25.0,24.0)]

first_t3 :: [(Double,Double)]
first_t3 = testSF2 (arr dup >>> first (arr (+1)))
first_t3r =
    [(1.0,0.0), (1.0,0.0), (1.0,0.0), (1.0,0.0), (1.0,0.0),
     (2.0,1.0), (2.0,1.0), (2.0,1.0), (2.0,1.0), (2.0,1.0),
     (3.0,2.0), (3.0,2.0), (3.0,2.0), (3.0,2.0), (3.0,2.0),
     (4.0,3.0), (4.0,3.0), (4.0,3.0), (4.0,3.0), (4.0,3.0),
     (5.0,4.0), (5.0,4.0), (5.0,4.0), (5.0,4.0), (5.0,4.0)]

first_t4 :: [(Double,Double)]
first_t4 = testSF1 (arr dup >>> first integral)
first_t4r =
    [(0.0,0.0),    (0.0,1.0),    (0.25,2.0),   (0.75,3.0),   (1.5,4.0),
     (2.5,5.0),    (3.75,6.0),   (5.25,7.0),   (7.0,8.0),    (9.0,9.0),
     (11.25,10.0), (13.75,11.0), (16.5,12.0),  (19.5,13.0),  (22.75,14.0),
     (26.25,15.0), (30.0,16.0),  (34.0,17.0),  (38.25,18.0), (42.75,19.0),
     (47.5,20.0),  (52.5,21.0),  (57.75,22.0), (63.25,23.0), (69.0,24.0)]

first_t5 :: [(Double,Double)]
first_t5 = testSF2 (arr dup >>> first integral)
first_t5r =
    [(0.0,0.0),  (0.0,0.0),  (0.0,0.0),  (0.0,0.0),  (0.0,0.0),
     (0.0,1.0),  (0.25,1.0), (0.5,1.0),  (0.75,1.0), (1.0,1.0),
     (1.25,2.0), (1.75,2.0), (2.25,2.0), (2.75,2.0), (3.25,2.0),
     (3.75,3.0), (4.5,3.0),  (5.25,3.0), (6.0,3.0),  (6.75,3.0),
     (7.5,4.0),  (8.5,4.0),  (9.5,4.0),  (10.5,4.0), (11.5,4.0)]

first_trs =
    [ first_t0 ~= first_t0r,
      first_t1 ~= first_t1r,
      first_t2 ~= first_t2r,
      first_t3 ~= first_t3r,
      first_t4 ~= first_t4r,
      first_t5 ~= first_t5r
    ]

first_tr = and first_trs


------------------------------------------------------------------------------
-- Test cases for second
------------------------------------------------------------------------------

-- These should mirror the test cases for first.

second_t0 :: [(Int,Double)]
second_t0 = testSF1 (arr dup >>> second (constant 7) >>> arr swap)

second_t1 :: [(Int,Double)]
second_t1 = testSF2 (arr dup >>> second (constant 7) >>> arr swap)

second_t2 :: [(Double,Double)]
second_t2 = testSF1 (arr dup >>> second (arr (+1)) >>> arr swap)

second_t3 :: [(Double,Double)]
second_t3 = testSF2 (arr dup >>> second (arr (+1)) >>> arr swap)

second_t4 :: [(Double,Double)]
second_t4 = testSF1 (arr dup >>> second integral >>> arr swap)

second_t5 :: [(Double,Double)]
second_t5 = testSF2 (arr dup >>> second integral >>> arr swap)

second_trs =
    [ second_t0 ~= first_t0r,
      second_t1 ~= first_t1r,
      second_t2 ~= first_t2r,
      second_t3 ~= first_t3r,
      second_t4 ~= first_t4r,
      second_t5 ~= first_t5r
    ]

second_tr = and second_trs
