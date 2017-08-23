{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsCOC.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsCOC					     *
*       Purpose:        Test cases for collection-oriented combinators	     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsCOC (coc_tr, coc_trs) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for collection-oriented combinators
------------------------------------------------------------------------------

coc_inp1 = deltaEncode 0.1 [0.0, 0.5 ..]

coc_t0 :: [[Double]]
coc_t0 = take 20 $ embed (parB [constant 1.0, identity, integral]) coc_inp1

coc_t0r =
    [[1.0, 0.0, 0.00],
     [1.0, 0.5, 0.00],
     [1.0, 1.0, 0.05],
     [1.0, 1.5, 0.15],
     [1.0, 2.0, 0.30],
     [1.0, 2.5, 0.50],
     [1.0, 3.0, 0.75],
     [1.0, 3.5, 1.05],
     [1.0, 4.0, 1.40],
     [1.0, 4.5, 1.80],
     [1.0, 5.0, 2.25],
     [1.0, 5.5, 2.75],
     [1.0, 6.0, 3.30],
     [1.0, 6.5, 3.90],
     [1.0, 7.0, 4.55],
     [1.0, 7.5, 5.25],
     [1.0, 8.0, 6.00],
     [1.0, 8.5, 6.80],
     [1.0, 9.0, 7.65],
     [1.0, 9.5, 8.55]]


coc_trs =
    [ coc_t0 ~= coc_t0r
    ]

coc_tr = and coc_trs
