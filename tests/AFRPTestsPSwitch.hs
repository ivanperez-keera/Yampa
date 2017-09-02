{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsPSwitch.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsPSwitch				     *
*       Purpose:        Test cases for pSwitchB and dpSwitchB		     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsPSwitch (
    pswitch_tr,
    pswitch_trs,
    pswitch_st0,
    pswitch_st0r,
    pswitch_st1,
    pswitch_st1r
) where

import Data.List (findIndex)

import FRP.Yampa
import FRP.Yampa.Internals (Event(NoEvent, Event))

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for pSwitchB and dpSwitchB
------------------------------------------------------------------------------

pswitch_inp1 = deltaEncode 0.1 [0.0, 0.5 ..]

whenFstGE :: Ord a => a -> c -> SF (a, b) (Event c)
whenFstGE a c = arr fst >>> arr (>= a) >>> edge >>> arr (`tag` c)

pswitch_t0 :: [[Double]]
pswitch_t0 = take 20 $ embed sf pswitch_inp1
    where
	sf =
	    pSwitchB [] (whenFstGE 1.25 10.0) $ \sfs x ->
	    pSwitchB (integral:sfs) (whenFstGE 3.75 10.0) $ \sfs x ->
	    pSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 5.25 20.0) $ \sfs x->
	    pSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 7.25 20.0) $ \sfs _->
	    parB (take 2 sfs)

pswitch_t0r =
    [[],			-- 0.0
     [],			-- 0.5
     [],			-- 1.0
     [0.0],			-- 1.5
     [0.15],			-- 2.0
     [0.35],			-- 2.5
     [0.60],			-- 3.0
     [0.90],			-- 3.5
     [10.00, 1.25],		-- 4.0
     [10.40, 1.65],		-- 4.5
     [10.85, 2.10],		-- 5.0
     [20.00, 11.35, 2.60],	-- 5.5
     [20.55, 11.90, 3.15],	-- 6.0
     [21.15, 12.50, 3.75],	-- 6.5
     [21.80, 13.15, 4.40],	-- 7.0
     [22.50, 13.85],		-- 7.5
     [23.25, 14.60],		-- 8.0
     [24.05, 15.40],		-- 8.5
     [24.90, 16.25],		-- 9.0
     [25.80, 17.15]]		-- 9.5


pswitch_t1 :: [[Double]]
pswitch_t1 = take 20 $ embed sf pswitch_inp1
    where
	sf =
	    dpSwitchB [] (whenFstGE 1.25 10.0) $ \sfs x ->
	    dpSwitchB (integral:sfs) (whenFstGE 3.75 10.0) $ \sfs x ->
	    dpSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 5.25 20.0)$ \sfs x->
	    dpSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 7.25 20.0)$ \sfs _->
	    parB (take 2 sfs)


pswitch_t1r =
    [[],			-- 0.0
     [],			-- 0.5
     [],			-- 1.0
     [],			-- 1.5
     [0.15],			-- 2.0
     [0.35],			-- 2.5
     [0.60],			-- 3.0
     [0.90],			-- 3.5
     [1.25],			-- 4.0
     [10.40, 1.65],		-- 4.5
     [10.85, 2.10],		-- 5.0
     [11.35, 2.60],		-- 5.5
     [20.55, 11.90, 3.15],	-- 6.0
     [21.15, 12.50, 3.75],	-- 6.5
     [21.80, 13.15, 4.40],	-- 7.0
     [22.50, 13.85, 5.10],	-- 7.5
     [23.25, 14.60],		-- 8.0
     [24.05, 15.40],		-- 8.5
     [24.90, 16.25],		-- 9.0
     [25.80, 17.15]]		-- 9.5


pswitch_t2 :: [[Double]]
pswitch_t2 = take 20 $ embed sf pswitch_inp1
    where
	sf =
	    pSwitchB [] (now 10.0) $ \sfs x ->
	    pSwitchB (integral:sfs) (whenFstGE 3.75 10.0) $ \sfs x ->
	    pSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 5.25 20.0) $ \sfs x->
	    pSwitchB ((integral>>>arr(+x)):sfs)(now 20.0) $ \sfs _->
	    parB (take 2 sfs)

pswitch_t2r =
    [[0.00],		-- 0.0
     [0.00],		-- 0.5
     [0.05],		-- 1.0
     [0.15],		-- 1.5
     [0.30],		-- 2.0
     [0.50],		-- 2.5
     [0.75],		-- 3.0
     [1.05],		-- 3.5
     [10.00,  1.40],	-- 4.0
     [10.40,  1.80],	-- 4.5
     [10.85,  2.25],	-- 5.0
     [20.00, 11.35],	-- 5.5
     [20.55, 11.90],	-- 6.0
     [21.15, 12.50],	-- 6.5
     [21.80, 13.15],	-- 7.0
     [22.50, 13.85],	-- 7.5
     [23.25, 14.60],	-- 8.0
     [24.05, 15.40],	-- 8.5
     [24.90, 16.25],	-- 9.0
     [25.80, 17.15]]	-- 9.5


pswitch_t3 :: [[Double]]
pswitch_t3 = take 20 $ embed sf pswitch_inp1
    where
	sf =
	    dpSwitchB [] (now 10.0) $ \sfs x ->
	    dpSwitchB (integral:sfs) (whenFstGE 3.75 10.0) $ \sfs x ->
	    dpSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 5.25 20.0)$ \sfs x->
	    dpSwitchB ((integral>>>arr(+x)):sfs) (now 20.0) $ \sfs _->
	    parB (take 2 sfs)

pswitch_t3r =
    [[],		-- 0.0
     [0.00],		-- 0.5
     [0.05],		-- 1.0
     [0.15],		-- 1.5
     [0.30],		-- 2.0
     [0.50],		-- 2.5
     [0.75],		-- 3.0
     [1.05],		-- 3.5
     [1.40],		-- 4.0
     [10.40,  1.80],	-- 4.5
     [10.85,  2.25],	-- 5.0
     [11.35,  2.75],	-- 5.5
     [20.55, 11.90],	-- 6.0
     [21.15, 12.50],	-- 6.5
     [21.80, 13.15],	-- 7.0
     [22.50, 13.85],	-- 7.5
     [23.25, 14.60],	-- 8.0
     [24.05, 15.40],	-- 8.5
     [24.90, 16.25],	-- 9.0
     [25.80, 17.15]]	-- 9.5


-- Starts three "ramps" with different phase. As soon as one exceeds a
-- threshold, it's restarted, while the others are left alone. The
-- observaton of the output is done via the loop (rather than the directly
-- from the outputs of the signal functions in the collection), thus the
-- use of a delayed switch is essential.

pswitch_ramp :: Double -> SF a Double
pswitch_ramp phase = constant 2.0 >>> integral >>> arr (+phase)

-- We assume that only one signal function will reach the limit at a time.
pswitch_limit :: Double -> SF ((a, [Double]), b) (Event Int)
pswitch_limit x = arr (snd . fst) >>> arr (findIndex (>=x)) >>> edgeJust

pswitch_t4 :: [[Double]]
pswitch_t4 = take 30 $ embed (loop sf) (deltaEncode 0.1 (repeat ()))
    where
        sf :: SF (a, [Double]) ([Double],[Double])
	sf = dpSwitchB [pswitch_ramp 0.0, pswitch_ramp 1.0, pswitch_ramp 2.0]
        	       (pswitch_limit 2.99)
		       pswitch_t4rec
	     >>> arr dup
        
pswitch_t4rec :: [SF (a, [Double]) Double]
                 -> Int
                 -> SF (a, [Double]) [Double]
pswitch_t4rec sfs n =
    dpSwitchB (take n sfs ++ [pswitch_ramp 0.0] ++ drop (n+1) sfs)
	      (pswitch_limit 2.99)
	      pswitch_t4rec

pswitch_t4r =
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


-- Variation of the test above, with direct observation (not via loop) and
-- immediate switch.

-- We assume that only one signal function will reach the limit at a time.
pswitch_limit2 :: Double -> SF (a, [Double]) (Event Int)
pswitch_limit2 x = arr snd >>> arr (findIndex (>=x)) >>> edgeJust

pswitch_t5 :: [([Double], Double)]
pswitch_t5 = take 30 $ embed (loop sf) (deltaEncode 0.1 (repeat ()))
    where
        sf :: SF (a, [Double]) (([Double], Double), [Double])
	sf = ((pSwitchB [pswitch_ramp 0.0, pswitch_ramp 1.0, pswitch_ramp 2.0]
        	        (pswitch_limit2 2.99)
		        pswitch_t5rec)
	      &&& (arr snd >>> arr sum))
	     >>> arr (\(xs, y) -> ((xs, y), xs))
        
pswitch_t5rec :: [SF (a, [Double]) Double]
                 -> Int
                 -> SF (a, [Double]) [Double]
pswitch_t5rec sfs n =
    pSwitchB (take n sfs ++ [pswitch_ramp 0.0] ++ drop (n+1) sfs)
	     (pswitch_limit2 2.99)
	     pswitch_t5rec

pswitch_t5r =
    [([0.0, 1.0, 2.0], 3.0),
     ([0.2, 1.2, 2.2], 3.6),
     ([0.4, 1.4, 2.4], 4.2),
     ([0.6, 1.6, 2.6], 4.8),
     ([0.8, 1.8, 2.8], 5.4),
     ([1.0, 2.0, 0.0], 3.0),
     ([1.2, 2.2, 0.2], 3.6),
     ([1.4, 2.4, 0.4], 4.2),
     ([1.6, 2.6, 0.6], 4.8),
     ([1.8, 2.8, 0.8], 5.4),
     ([2.0, 0.0, 1.0], 3.0),
     ([2.2, 0.2, 1.2], 3.6),
     ([2.4, 0.4, 1.4], 4.2),
     ([2.6, 0.6, 1.6], 4.8),
     ([2.8, 0.8, 1.8], 5.4),
     ([0.0, 1.0, 2.0], 3.0),
     ([0.2, 1.2, 2.2], 3.6),
     ([0.4, 1.4, 2.4], 4.2),
     ([0.6, 1.6, 2.6], 4.8),
     ([0.8, 1.8, 2.8], 5.4),
     ([1.0, 2.0, 0.0], 3.0),
     ([1.2, 2.2, 0.2], 3.6),
     ([1.4, 2.4, 0.4], 4.2),
     ([1.6, 2.6, 0.6], 4.8),
     ([1.8, 2.8, 0.8], 5.4),
     ([2.0, 0.0, 1.0], 3.0),
     ([2.2, 0.2, 1.2], 3.6),
     ([2.4, 0.4, 1.4], 4.2),
     ([2.6, 0.6, 1.6], 4.8),
     ([2.8, 0.8, 1.8], 5.4)]


pswitch_trs =
    [ pswitch_t0 ~= pswitch_t0r,
      pswitch_t1 ~= pswitch_t1r,
      pswitch_t2 ~= pswitch_t2r,
      pswitch_t3 ~= pswitch_t3r,
      pswitch_t4 ~= pswitch_t4r,
      pswitch_t5 ~= pswitch_t5r
    ]

pswitch_tr = and pswitch_trs


pswitch_st0 = testSFSpaceLeak 1000000 (loop sf)
    where
        sf :: SF (a, [Double]) ([Double],[Double])
	sf = dpSwitchB [pswitch_ramp 0.0, pswitch_ramp 1.0, pswitch_ramp 2.0]
        	       (pswitch_limit 2.99)
		       pswitch_t4rec
	     >>> arr dup

pswitch_st0r = [1.5,2.5,0.5]


pswitch_st1 = testSFSpaceLeak 1000000 (loop sf)
    where
        sf :: SF (a, [Double]) (([Double], Double), [Double])
	sf = ((pSwitchB [pswitch_ramp 0.0, pswitch_ramp 1.0, pswitch_ramp 2.0]
        	        (pswitch_limit2 2.99)
		        pswitch_t5rec)
	      &&& (arr snd >>> arr sum))
	     >>> arr (\(xs, y) -> ((xs, y), xs))

pswitch_st1r = ([1.5,2.5,0.5],4.5)
