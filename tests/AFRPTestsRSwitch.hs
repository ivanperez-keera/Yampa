{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsRSwitch.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsRSwitch				     *
*       Purpose:        Test cases for rSwitch and drSwitch		     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsRSwitch (
    rswitch_tr,
    rswitch_trs,
    rswitch_st0,
    rswitch_st0r
) where

import Data.Maybe (fromJust)

import FRP.Yampa
import FRP.Yampa.Internals (Event(NoEvent, Event))

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for rSwitch and drSwitch
------------------------------------------------------------------------------

rswitch_inp1 = (fromJust (head delta_inp), zip (repeat 1.0) (tail delta_inp))
    where
	delta_inp =
            [Just (1.0, NoEvent), Nothing, Nothing,
             Just (2.0, Event (arr (*3))), Just (3.0, NoEvent), Nothing,
             Just (4.0, NoEvent), Nothing, Nothing,
             Just (5.0, Event integral),
             Just (6.0, NoEvent), Nothing,
             Just (7.0, NoEvent), Nothing, Nothing,
             Just (8.0, Event (arr (*7))), Just (9.0, NoEvent), Nothing]
            ++ repeat Nothing


-- This input contains exaples of "continuos switching", i.e. the same
-- switching event ocurring during a a few contiguous time steps.
-- It also starts with an immediate switch.
rswitch_inp2 = (fromJust (head delta_inp), zip (repeat 1.0) (tail delta_inp))
    where
        delta_inp =
            [Just (1.0, Event integral),
             Just (1.0, NoEvent), Nothing,
             Just (2.0, Event (arr (*2))), Nothing, Nothing,
             Just (3.0, Event integral), Nothing, Nothing,
             Just (4.0, NoEvent), Nothing, Nothing,
             Just (5.0, Event integral),
             Just (5.0, NoEvent), Nothing,
             Just (6.0, Event (arr (*3))), Just (7.0, Event (arr (*4))),
             Just (8.0, Event integral),
             Just (9.0, NoEvent), Nothing]
            ++ repeat Nothing


rswitch_t0 = take 20 $ embed (rSwitch (arr (+3))) rswitch_inp1

-- Integration using rectangle rule assumed.
rswitch_t0r :: [Double]
rswitch_t0r =
    [4.0,  4.0,  4.0,  6.0,  9.0,
     9.0,  12.0, 12.0, 12.0, 0.0,
     5.0,  11.0, 17.0, 24.0, 31.0,
     56.0, 63.0, 63.0, 63.0, 63.0]


rswitch_t1 = take 20 $ embed (rSwitch integral) rswitch_inp1

-- Integration using rectangle rule assumed.
rswitch_t1r :: [Double]
rswitch_t1r =
    [0.0,  1.0,  2.0,  6.0,  9.0,
     9.0,  12.0, 12.0, 12.0, 0.0,
     5.0,  11.0, 17.0, 24.0, 31.0,
     56.0, 63.0, 63.0, 63.0, 63.0]

rswitch_t2 = take 20 $ embed (rSwitch (arr (+100))) rswitch_inp2

-- Integration using rectangle rule assumed.
rswitch_t2r :: [Double]
rswitch_t2r =
    [0.0,  1.0,  2.0, 4.0, 4.0,
     4.0,  0.0,  0.0, 0.0, 3.0,
     7.0,  11.0, 0.0, 5.0, 10.0,
     18.0, 28.0, 0.0, 8.0, 17.0]


rswitch_t3 = take 20 $ embed (drSwitch (arr (+100))) rswitch_inp2

-- Integration using rectangle rule assumed.
rswitch_t3r :: [Double]
rswitch_t3r =
    [101.0, 1.0,  2.0,  3.0, 4.0,
     4.0,   6.0,  3.0,  3.0, 3.0,
     7.0,   11.0, 15.0, 5.0, 10.0,
     15.0,  21.0, 32.0, 8.0, 17.0]


rswitch_sawTooth :: SF a Double
rswitch_sawTooth =
    loop (second (arr (>=5.0)
                  >>> edge
                  >>> arr (`tag` ramp))
          >>> drSwitch ramp
          >>> arr dup)
    where
        ramp :: SF a Double
        ramp = constant 1.0 >>> integral

rswitch_inp3 = deltaEncode 0.5 (repeat 0.0)

rswitch_t4 = take 40 $ embed rswitch_sawTooth rswitch_inp3

rswitch_t4r =
    [0.0, 0.5, 1.0, 1.5, 2.0,
     2.5, 3.0, 3.5, 4.0, 4.5,
     5.0, 0.5, 1.0, 1.5, 2.0,
     2.5, 3.0, 3.5, 4.0, 4.5,
     5.0, 0.5, 1.0, 1.5, 2.0,
     2.5, 3.0, 3.5, 4.0, 4.5,
     5.0, 0.5, 1.0, 1.5, 2.0,
     2.5, 3.0, 3.5, 4.0, 4.5]

rswitch_trs =
    [ rswitch_t0 ~= rswitch_t0r,
      rswitch_t1 ~= rswitch_t1r,
      rswitch_t2 ~= rswitch_t2r,
      rswitch_t3 ~= rswitch_t3r,
      rswitch_t4 ~= rswitch_t4r
    ]

rswitch_tr = and rswitch_trs


rswitch_st0 = testSFSpaceLeak 2000000 rswitch_sawTooth
rswitch_st0r = 4.75
