{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsRPSwitch.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsRPSwitch				     *
*       Purpose:        Test cases for rpSwitchB and drpSwitchB		     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsRPSwitch (
    rpswitch_tr,
    rpswitch_trs,
    rpswitch_st0,
    rpswitch_st0r
) where

import Data.Maybe (fromJust)
import Data.List (findIndex)

import FRP.Yampa
import FRP.Yampa.Internals (Event(NoEvent, Event))

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for rpSwitchB and drpSwitchB
------------------------------------------------------------------------------

rpswitch_inp1 = (fromJust (head delta_inp), zip (repeat 1.0) (tail delta_inp))
    where
	delta_inp =
            [Just (1.0, NoEvent), Nothing, Nothing,
             Just (2.0, Event (integral:)), Just (3.0, NoEvent), Nothing,
             Just (4.0, NoEvent), Nothing, Nothing,
             Just (5.0, Event ((integral >>> arr (+100.0)):)),
             Just (6.0, NoEvent), Nothing,
             Just (7.0, NoEvent), Nothing, Nothing,
             Just (8.0, Event tail), Just (9.0, NoEvent), Nothing]
            ++ repeat Nothing


-- This input contains exaples of "continuos switching", i.e. the same
-- switching event ocurring during a a few contiguous time steps.
-- It also starts with an immediate switch.
rpswitch_inp2 = (fromJust (head delta_inp), zip (repeat 1.0) (tail delta_inp))
    where
        delta_inp =
            [Just (1.0, Event (integral:)),
             Just (1.0, NoEvent), Nothing,
             Just (2.0, Event ((integral >>> arr(+100.0)):)), Nothing, Nothing,
             Just (3.0, Event ((integral >>> arr(+200.0)):)), Nothing, Nothing,
             Just (4.0, NoEvent), Nothing, Nothing,
             Just (5.0, Event ((arr (*3)):)),
             Just (5.0, NoEvent), Nothing,
             Just (6.0, Event tail), Just (7.0, Event ((arr (*7)):)),
             Just (8.0, Event (take 2)),
             Just (9.0, NoEvent), Nothing]
            ++ repeat Nothing


rpswitch_t0 :: [[Double]]
rpswitch_t0 = take 20 $ embed (rpSwitchB []) rpswitch_inp1

rpswitch_t0r =
    [[],		-- 0 s
     [],		-- 1 s
     [],		-- 2 s
     [0.0],		-- 3 s
     [2.0],		-- 4 s
     [5.0],		-- 5 s
     [8.0],		-- 6 s
     [12.0],		-- 7 s
     [16.0],		-- 8 s
     [100.0, 20.0],	-- 9 s
     [105.0, 25.0],	-- 10 s
     [111.0, 31.0],	-- 11 s
     [117.0, 37.0],	-- 12 s
     [124.0, 44.0],	-- 13 s
     [131.0, 51.0],	-- 14 s
     [58.0],		-- 15 s
     [66.0],		-- 16 s
     [75.0],		-- 17 s
     [84.0],		-- 18 s
     [93.0]]		-- 19 s


rpswitch_t1 :: [[Double]]
rpswitch_t1 = take 20 $ embed (drpSwitchB []) rpswitch_inp1

rpswitch_t1r =
    [[],		-- 0 s 
     [],		-- 1 s 
     [],		-- 2 s 
     [],		-- 3 s 
     [2.0],		-- 4 s 
     [5.0],		-- 5 s 
     [8.0],		-- 6 s 
     [12.0],		-- 7 s 
     [16.0],		-- 8 s 
     [20.0]	,	-- 9 s 
     [105.0, 25.0],	-- 10 s
     [111.0, 31.0],	-- 11 s
     [117.0, 37.0],	-- 12 s
     [124.0, 44.0],	-- 13 s
     [131.0, 51.0],	-- 14 s
     [138.0, 58.0],	-- 15 s
     [66.0],		-- 16 s
     [75.0],		-- 17 s
     [84.0],		-- 18 s
     [93.0]]		-- 19 s


rpswitch_t2 :: [[Double]]
rpswitch_t2 = take 20 $ embed (rpSwitchB []) rpswitch_inp2

rpswitch_t2r =
    [[0.0],							-- 0 s 
     [1.0],							-- 1 s 
     [2.0],							-- 2 s 
     [100.0, 3.0],						-- 3 s 
     [100.0, 102.0, 5.0],					-- 4 s 
     [100.0, 102.0, 104.0, 7.0],				-- 5 s 
     [200.0, 102.0, 104.0, 106.0, 9.0],				-- 6 s 
     [200.0, 203.0, 105.0, 107.0, 109.0, 12.0],			-- 7 s 
     [200.0, 203.0, 206.0, 108.0, 110.0, 112.0, 15.0],		-- 8 s 
     [203.0, 206.0, 209.0, 111.0, 113.0, 115.0, 18.0],		-- 9 s 
     [207.0, 210.0, 213.0, 115.0, 117.0, 119.0, 22.0],		-- 10 s
     [211.0, 214.0, 217.0, 119.0, 121.0, 123.0, 26.0],		-- 11 s
     [15.0, 215.0, 218.0, 221.0, 123.0, 125.0, 127.0, 30.0],	-- 12 s
     [15.0, 220.0, 223.0, 226.0, 128.0, 130.0, 132.0, 35.0],	-- 13 s
     [15.0, 225.0, 228.0, 231.0, 133.0, 135.0, 137.0, 40.0],	-- 14 s
     [230.0, 233.0, 236.0, 138.0, 140.0, 142.0, 45.0],		-- 15 s
     [49.0, 236.0, 239.0, 242.0, 144.0, 146.0, 148.0, 51.0],	-- 16 s
     [56.0, 243.0],						-- 17 s
     [63.0, 251.0],						-- 18 s
     [63.0, 260.0]]						-- 19 s


rpswitch_t3 :: [[Double]]
rpswitch_t3 = take 20 $ embed (drpSwitchB []) rpswitch_inp2

rpswitch_t3r =
    [[],							-- 0 s 
     [1.0],							-- 1 s 
     [2.0],							-- 2 s 
     [3.0],							-- 3 s 
     [102.0, 5.0],						-- 4 s 
     [102.0, 104.0, 7.0],					-- 5 s 
     [102.0, 104.0, 106.0, 9.0],				-- 6 s 
     [203.0, 105.0, 107.0, 109.0, 12.0],			-- 7 s 
     [203.0, 206.0, 108.0, 110.0, 112.0, 15.0],			-- 8 s 
     [203.0, 206.0, 209.0, 111.0, 113.0, 115.0, 18.0],		-- 9 s 
     [207.0, 210.0, 213.0, 115.0, 117.0, 119.0, 22.0],		-- 10 s
     [211.0, 214.0, 217.0, 119.0, 121.0, 123.0, 26.0],		-- 11 s
     [215.0, 218.0, 221.0, 123.0, 125.0, 127.0, 30.0],		-- 12 s
     [15.0, 220.0, 223.0, 226.0, 128.0, 130.0, 132.0, 35.0],	-- 13 s
     [15.0, 225.0, 228.0, 231.0, 133.0, 135.0, 137.0, 40.0],	-- 14 s
     [18.0, 230.0, 233.0, 236.0, 138.0, 140.0, 142.0, 45.0],	-- 15 s
     [236.0, 239.0, 242.0, 144.0, 146.0, 148.0, 51.0],		-- 16 s
     [56.0, 243.0, 246.0, 249.0, 151.0, 153.0, 155.0, 58.0],	-- 17 s
     [63.0, 251.0],						-- 18 s
     [63.0, 260.0]]						-- 19 s


-- Starts three "ramps" with different phase. As soon as one exceeds a
-- threshold, it's restarted, while the others are left alone. The observaton
-- of the output is done via a loop, thus the  use of a delayed switch is
-- essential.

rpswitch_ramp :: Double -> SF a Double
rpswitch_ramp phase = constant 2.0 >>> integral >>> arr (+phase)

-- We assume that only one signal function will reach the limit at a time.
rpswitch_limit :: Double -> SF [Double] (Event ([SF a Double]->[SF a Double]))
rpswitch_limit x = arr (findIndex (>=x)) >>> edgeJust >>> arr (fmap restart)
    where
	restart n = \sfs -> take n sfs ++ [rpswitch_ramp 0.0] ++ drop (n+1) sfs

rpswitch_t4 :: [[Double]]
rpswitch_t4 = take 30 $ embed (loop sf) (deltaEncode 0.1 (repeat ()))
    where
        sf :: SF (a, [Double]) ([Double],[Double])
	sf = (second (rpswitch_limit 2.99)
	      >>> drpSwitchB [rpswitch_ramp 0.0,
			      rpswitch_ramp 1.0,
			      rpswitch_ramp 2.0])
	     >>> arr dup

rpswitch_t4r =
    [[0.0, 1.0, 2.0],
     [0.2, 1.2, 2.2],
     [0.4, 1.4, 2.4],
     [0.6, 1.6, 2.6],
     [0.8, 1.8, 2.8],
     [1.0, 2.0, 3.0],
     [1.2, 2.2, 0.2],
     [1.4, 2.4, 0.4],
     [1.6, 2.6, 0.6],
     [1.8, 2.8, 0.8],
     [2.0, 3.0, 1.0],
     [2.2, 0.2, 1.2],
     [2.4, 0.4, 1.4],
     [2.6, 0.6, 1.6],
     [2.8, 0.8, 1.8],
     [3.0, 1.0, 2.0],
     [0.2, 1.2, 2.2],
     [0.4, 1.4, 2.4],
     [0.6, 1.6, 2.6],
     [0.8, 1.8, 2.8],
     [1.0, 2.0, 3.0],
     [1.2, 2.2, 0.2],
     [1.4, 2.4, 0.4],
     [1.6, 2.6, 0.6],
     [1.8, 2.8, 0.8],
     [2.0, 3.0, 1.0],
     [2.2, 0.2, 1.2],
     [2.4, 0.4, 1.4],
     [2.6, 0.6, 1.6],
     [2.8, 0.8, 1.8]]


rpswitch_trs =
    [ rpswitch_t0 ~= rpswitch_t0r,
      rpswitch_t1 ~= rpswitch_t1r,
      rpswitch_t2 ~= rpswitch_t2r,
      rpswitch_t3 ~= rpswitch_t3r,
      rpswitch_t4 ~= rpswitch_t4r
    ]

rpswitch_tr = and rpswitch_trs


rpswitch_st0 = testSFSpaceLeak 1000000 (loop sf)
    where
        sf :: SF (a, [Double]) ([Double],[Double])
	sf = (second (rpswitch_limit 2.99)
	      >>> drpSwitchB [rpswitch_ramp 0.0,
			      rpswitch_ramp 1.0,
			      rpswitch_ramp 2.0])
	     >>> arr dup

rpswitch_st0r = [1.5,2.5,0.5]
