{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsLoopPre.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsLoopPre				     *
*       Purpose:        Test cases for loopPre				     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsLoopPre (loopPre_tr, loopPre_trs) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for loopPre
------------------------------------------------------------------------------

loop_acc :: SF (Double, Double) (Double, Double)
loop_acc = arr (\(x, y)->(x+y, x+y))

-- This kind of test will fail for infinitesimal delay!
loopPre_t0 = testSF1 (loopPre 0 loop_acc)
loopPre_t0r =
    [0.0,1.0,3.0,6.0,10.0,15.0,21.0,28.0,36.0,45.0,55.0,66.0,78.0,91.0,
     105.0,120.0,136.0,153.0,171.0,190.0,210.0,231.0,253.0,276.0,300.0]

loopPre_t1 = testSF2 (loopPre 0 loop_acc)
loopPre_t1r =
    [0.0,0.0,0.0,0.0,0.0,1.0,2.0,3.0,4.0,5.0,7.0,9.0,11.0,13.0,15.0,18.0,
     21.0,24.0,27.0,30.0,34.0,38.0,42.0,46.0,50.0]

-- This kind of test will fail for infinitesimal delay!
loopPre_t2 = testSF1 (loopPre False (arr (dup . not . snd)))
loopPre_t2r =
    [True,False,True,False,True,False,True,False,True,False,True,False,
     True,False,True,False,True,False,True,False,True,False,True,False,True]

loopPre_t3 = testSF1 (loopPre 0 (first localTime))
loopPre_t3r =
    [0.0,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,
     4.0,4.25,4.5,4.75,5.0,5.25,5.5,5.75,6.0]

loopPre_t4 = testSF1 (loopPre 0 (first localTime >>> loop_acc))
loopPre_t4r =
    [0.0,0.25,0.75,1.5,2.5,3.75,5.25,7.0,9.0,11.25,13.75,16.5,19.5,22.75,
     26.25,30.0,34.0,38.25,42.75,47.5,52.5,57.75,63.25,69.0,75.0]

loopPre_trs =
    [ loopPre_t0 ~= loopPre_t0r,
      loopPre_t1 ~= loopPre_t1r,
      loopPre_t2 ~= loopPre_t2r,
      loopPre_t3 ~= loopPre_t3r,
      loopPre_t4 ~= loopPre_t4r
    ]

loopPre_tr = and loopPre_trs
