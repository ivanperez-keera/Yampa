{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsWFG.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsWFG					     *
*       Purpose:        Test cases for wave-form generation		     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsWFG (wfg_tr, wfg_trs) where

import FRP.Yampa
import FRP.Yampa.Internals (Event(NoEvent, Event))

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for wave-form generation
------------------------------------------------------------------------------

wfg_inp1 = deltaEncode 1.0 $
    [NoEvent,   NoEvent,   Event 1.0, NoEvent,
     Event 2.0, NoEvent,   NoEvent,   NoEvent,
     Event 3.0, Event 4.0, Event 4.0, NoEvent,
     Event 0.0, NoEvent,   NoEvent,   NoEvent]
    ++ repeat NoEvent


wfg_inp2 = deltaEncode 1.0 $
    [Event 1.0, NoEvent,   NoEvent,   NoEvent,
     Event 2.0, NoEvent,   NoEvent,   NoEvent,
     Event 3.0, Event 4.0, Event 4.0, NoEvent,
     Event 0.0, NoEvent,   NoEvent,   NoEvent]
    ++ repeat NoEvent


wfg_t0 :: [Double]
wfg_t0 = take 16 $ embed (hold 99.99) wfg_inp1

wfg_t0r =
    [99.99, 99.99, 1.0, 1.0,
     2.0,   2.0,   2.0, 2.0,
     3.0,   4.0,   4.0, 4.0,
     0.0,   0.0,   0.0, 0.0]

wfg_t1 :: [Double]
wfg_t1 = take 16 $ embed (hold 99.99) wfg_inp2

wfg_t1r =
    [1.0, 1.0, 1.0, 1.0,
     2.0, 2.0, 2.0, 2.0,
     3.0, 4.0, 4.0, 4.0,
     0.0, 0.0, 0.0, 0.0]

wfg_inp3 = deltaEncode 1.0 $
    [Nothing,  Nothing,  Just 1.0, Just 2.0, Just 3.0,
     Just 4.0, Nothing,  Nothing,  Nothing,  Just 3.0,
     Just 2.0, Nothing,  Just 1.0, Just 0.0, Just 1.0,
     Just 2.0, Just 3.0, Nothing,  Nothing,  Just 4.0]
    ++ repeat Nothing

wfg_inp4 = deltaEncode 1.0 $
    [Just 0.0, Nothing,  Just 1.0, Just 2.0, Just 3.0,
     Just 4.0, Nothing,  Nothing,  Nothing,  Just 3.0,
     Just 2.0, Nothing,  Just 1.0, Just 0.0, Just 1.0,
     Just 2.0, Just 3.0, Nothing,  Nothing,  Just 4.0]
    ++ repeat Nothing


wfg_t2 :: [Double]
wfg_t2 = take 25 $ embed (trackAndHold 99.99) wfg_inp3

wfg_t2r =
    [99.99, 99.99, 1.0, 2.0, 3.0,
     4.0,   4.0,   4.0, 4.0, 3.0,
     2.0,   2.0,   1.0, 0.0, 1.0,
     2.0,   3.0,   3.0, 3.0, 4.0,
     4.0,   4.0,   4.0, 4.0, 4.0]


wfg_t3 :: [Double]
wfg_t3 = take 25 $ embed (trackAndHold 99.99) wfg_inp4

wfg_t3r =
    [0.0, 0.0, 1.0, 2.0, 3.0,
     4.0, 4.0, 4.0, 4.0, 3.0,
     2.0, 2.0, 1.0, 0.0, 1.0,
     2.0, 3.0, 3.0, 3.0, 4.0,
     4.0, 4.0, 4.0, 4.0, 4.0]


wfg_trs =
    [ wfg_t0 ~= wfg_t0r,
      wfg_t1 ~= wfg_t1r,
      wfg_t2 ~= wfg_t2r,
      wfg_t3 ~= wfg_t3r
    ]

wfg_tr = and wfg_trs
