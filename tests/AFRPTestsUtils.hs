{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsUtils.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsUtils					     *
*       Purpose:        Test cases for utilities (AFRPUtilities)	     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

-- Not really intended to test all definitions in the utilities module.

module AFRPTestsUtils (utils_tr, utils_trs) where

import FRP.Yampa
import FRP.Yampa.Internals (Event(NoEvent, Event))
import FRP.Yampa.Conditional
import FRP.Yampa.Integration
import FRP.Yampa.EventS
import FRP.Yampa.Hybrid
import FRP.Yampa.Utilities
import FRP.Yampa.Switches

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for utilities (AFRPUtils)
------------------------------------------------------------------------------

-- Should re-order these test cases to reflect the order in AFRPUtils
-- at some point.

utils_inp1 = deltaEncode 1.0 $
    [NoEvent,   NoEvent,   Event 1.0, NoEvent,
     Event 2.0, NoEvent,   NoEvent,   NoEvent,
     Event 3.0, Event 4.0, Event 4.0, NoEvent,
     Event 0.0, NoEvent,   NoEvent,   NoEvent]
    ++ repeat NoEvent


utils_inp2 = deltaEncode 1.0 $
    [Event 1.0, NoEvent,   NoEvent,   NoEvent,
     Event 2.0, NoEvent,   NoEvent,   NoEvent,
     Event 3.0, Event 4.0, Event 4.0, NoEvent,
     Event 0.0, NoEvent,   NoEvent,   NoEvent]
    ++ repeat NoEvent


utils_t0 :: [Double]
utils_t0 = take 16 $ embed (dHold 99.99) utils_inp1

utils_t0r =
    [99.99, 99.99, 99.99, 1.0,
     1.0,   2.0,   2.0,   2.0,
     2.0,   3.0,   4.0,   4.0,
     4.0,   0.0,   0.0,   0.0]

utils_t1 :: [Double]
utils_t1 = take 16 $ embed (dHold 99.99) utils_inp2

utils_t1r =
    [99.99, 1.0, 1.0, 1.0,
     1.0,   2.0, 2.0, 2.0,
     2.0,   3.0, 4.0, 4.0,
     4.0,   0.0, 0.0, 0.0]


utils_inp3 = deltaEncode 1.0 $
    [Nothing,  Nothing,  Just 1.0, Just 2.0, Just 3.0,
     Just 4.0, Nothing,  Nothing,  Nothing,  Just 3.0,
     Just 2.0, Nothing,  Just 1.0, Just 0.0, Just 1.0,
     Just 2.0, Just 3.0, Nothing,  Nothing,  Just 4.0]
    ++ repeat Nothing

utils_inp4 = deltaEncode 1.0 $
    [Just 0.0, Nothing,  Just 1.0, Just 2.0, Just 3.0,
     Just 4.0, Nothing,  Nothing,  Nothing,  Just 3.0,
     Just 2.0, Nothing,  Just 1.0, Just 0.0, Just 1.0,
     Just 2.0, Just 3.0, Nothing,  Nothing,  Just 4.0]
    ++ repeat Nothing


utils_t2 :: [Double]
utils_t2 = take 25 $ embed (dTrackAndHold 99.99) utils_inp3

utils_t2r =
    [99.99, 99.99, 99.99, 1.0, 2.0,
     3.0,   4.0,   4.0,   4.0, 4.0,
     3.0,   2.0,   2.0,   1.0, 0.0,
     1.0,   2.0,   3.0,   3.0, 3.0,
     4.0,   4.0,   4.0,   4.0, 4.0]

utils_t3 :: [Double]
utils_t3 = take 25 $ embed (dTrackAndHold 99.99) utils_inp4

utils_t3r =
    [99.99, 0.0, 0.0, 1.0, 2.0,
     3.0,   4.0, 4.0, 4.0, 4.0,
     3.0,   2.0, 2.0, 1.0, 0.0,
     1.0,   2.0, 3.0, 3.0, 3.0,
     4.0,   4.0, 4.0, 4.0, 4.0]


utils_t4 :: [Event Int]
utils_t4 = take 16 $ embed count utils_inp1

utils_t4r :: [Event Int]
utils_t4r = 
    [NoEvent, NoEvent, Event 1, NoEvent,
     Event 2, NoEvent, NoEvent, NoEvent,
     Event 3, Event 4, Event 5, NoEvent,
     Event 6, NoEvent, NoEvent, NoEvent]


utils_t5 :: [Event Int]
utils_t5 = take 16 $ embed count utils_inp2

utils_t5r :: [Event Int]
utils_t5r = 
    [Event 1, NoEvent, NoEvent, NoEvent,
     Event 2, NoEvent, NoEvent, NoEvent,
     Event 3, Event 4, Event 5, NoEvent,
     Event 6, NoEvent, NoEvent, NoEvent]


dynDelayLine :: a -> SF (a, Event Bool) a
dynDelayLine a0 =
    second (arr (fmap (\p -> if p then addDelay else delDelay)))
    >>> loop (arr (\((a, e), as) -> (a:as, e))
              >>> rpSwitchZ [iPre a0]
              >>> arr (\as -> (last as, init as)))
    where
	addDelay ds = ds ++ [last ds]

        delDelay [d] = [d]
        delDelay ds  = init ds

utils_t6 :: [Int]
utils_t6 = take 200 $ embed (dynDelayLine 0)
			    (deltaEncode 0.1 (zip [1..] evSeq))
    where
	evSeq = NoEvent : Event True : NoEvent : NoEvent : Event True :
		NoEvent : NoEvent : Event False : evSeq

utils_t6r =
    [0,1,1,2,3,3,4,6,7,8,8,9,10,10,11,13,14,15,15,16,17,17,18,20,21,22,22,23,
     24,24,25,27,28,29,29,30,31,31,32,34,35,36,36,37,38,38,39,41,42,43,43,44,
     45,45,46,48,49,50,50,51,52,52,53,55,56,57,57,58,59,59,60,62,63,64,64,65,
     66,66,67,69,70,71,71,72,73,73,74,76,77,78,78,79,80,80,81,83,84,85,85,86,
     87,87,88,90,91,92,92,93,94,94,95,97,98,99,99,100,101,101,102,104,105,106,
     106,107,108,108,109,111,112,113,113,114,115,115,116,118,119,120,120,121,
     122,122,123,125,126,127,127,128,129,129,130,132,133,134,134,135,136,136,
     137,139,140,141,141,142,143,143,144,146,147,148,148,149,150,150,151,153,
     154,155,155,156,157,157,158,160,161,162,162,163,164,164,165,167,168,169,
     169,170,171,171,172,174]

utils_t7 :: [Double]
utils_t7 = take 50 $ embed impulseIntegral
                           (deltaEncode 0.1 (zip (repeat 1.0) evSeq))
    where
	evSeq = replicate 9 NoEvent ++ [Event 10.0]
		++ replicate 9 NoEvent ++ [Event (-10.0)]
		++ evSeq

utils_t7r =
    [ 0.0,  0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8, 10.9,
     11.0, 11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7, 11.8,  1.9,
      2.0,  2.1,  2.2,  2.3,  2.4,  2.5,  2.6,  2.7,  2.8, 12.9,
     13.0, 13.1, 13.2, 13.3, 13.4, 13.5, 13.6, 13.7, 13.8,  3.9,
      4.0,  4.1,  4.2,  4.3,  4.4,  4.5,  4.6,  4.7,  4.8, 14.9]


utils_t8 :: [Double]
utils_t8 = take 50 $ embed (provided (even . floor) integral (constant (-1)))
                           (deltaEncode 0.1 input)
    where
	input = replicate 10 1
		++ replicate 10 2
		++ replicate 10 3
		++ replicate 10 4
		++ input

utils_t8r =
    [-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
      0.0,  0.2,  0.4,  0.6,  0.8,  1.0,  1.2,  1.4,  1.6,  1.8,
     -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
      0.0,  0.4,  0.8,  1.2,  1.6,  2.0,  2.4,  2.8,  3.2,  3.6,
     -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0]


utils_t9 :: [Double]
utils_t9 = take 50 $ embed (provided (odd . floor) integral (constant (-1)))
                           (deltaEncode 0.1 input)
    where
	input = replicate 10 1
		++ replicate 10 2
		++ replicate 10 3
		++ replicate 10 4
		++ input

utils_t9r =
    [ 0.0,  0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8,  0.9,
     -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
      0.0,  0.3,  0.6,  0.9,  1.2,  1.5,  1.8,  2.1,  2.4,  2.7,
     -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
      0.0,  0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8,  0.9]


utils_t10 :: [Event Double]
utils_t10 = testSF1 snap

utils_t10r =
    [Event 0.0, NoEvent, NoEvent, NoEvent,	-- 0.0 s
     NoEvent,   NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent,   NoEvent, NoEvent, NoEvent,	-- 2.0 s
     NoEvent,   NoEvent, NoEvent, NoEvent,	-- 3.0 s
     NoEvent,   NoEvent, NoEvent, NoEvent,	-- 4.0 s
     NoEvent,   NoEvent, NoEvent, NoEvent,	-- 5.0 s
     NoEvent]


utils_t11 :: [Event Double]
utils_t11 = testSF1 (snapAfter 2.6)

utils_t11r =
    [NoEvent, NoEvent, NoEvent, NoEvent,	-- 0.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent, NoEvent, NoEvent, Event 11.0,	-- 2.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 3.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 4.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 5.0 s
     NoEvent]


utils_t12 :: [Event Double]
utils_t12 = testSF1 (sample 0.99)

utils_t12r =
    [NoEvent,    NoEvent, NoEvent, NoEvent,	-- 0.0 s
     Event 4.0,  NoEvent, NoEvent, NoEvent,	-- 1.0 s
     Event 8.0,  NoEvent, NoEvent, NoEvent,	-- 2.0 s
     Event 12.0, NoEvent, NoEvent, NoEvent,	-- 3.0 s
     Event 16.0, NoEvent, NoEvent, NoEvent,	-- 4.0 s
     Event 20.0, NoEvent, NoEvent, NoEvent,	-- 5.0 s
     Event 24.0]


utils_t13 :: [Event ()]
utils_t13 = testSF1 (recur (after 0.99 ()))

utils_t13r =
    [NoEvent,  NoEvent, NoEvent, NoEvent,	-- 0.0 s
     Event (), NoEvent, NoEvent, NoEvent,	-- 1.0 s
     Event (), NoEvent, NoEvent, NoEvent,	-- 2.0 s
     Event (), NoEvent, NoEvent, NoEvent,	-- 3.0 s
     Event (), NoEvent, NoEvent, NoEvent,	-- 4.0 s
     Event (), NoEvent, NoEvent, NoEvent,	-- 5.0 s
     Event ()]


utils_t14 :: [Event Int]
utils_t14 = testSF1 (after 1.0 1 `andThen` now 2 `andThen` after 2.0 3)

utils_t14r =
    [NoEvent, NoEvent, NoEvent, NoEvent,	-- 0.0 s
     Event 1, NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 2.0 s
     Event 3, NoEvent, NoEvent, NoEvent,	-- 3.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 4.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 5.0 s
     NoEvent]

utils_t15 = take 50 (embed (time >>> sampleWindow 5 0.5)
                           (deltaEncode 0.125 (repeat ())))

utils_t15r =
    [ NoEvent,                     NoEvent, NoEvent, NoEvent,	-- 0.0 s
      Event [0.5],                 NoEvent, NoEvent, NoEvent,	-- 0.5 s
      Event [0.5,1.0],             NoEvent, NoEvent, NoEvent,	-- 1.0 s
      Event [0.5,1.0,1.5],         NoEvent, NoEvent, NoEvent,	-- 1.5 s
      Event [0.5,1.0,1.5,2.0],     NoEvent, NoEvent, NoEvent, 	-- 2.0 s
      Event [0.5,1.0,1.5,2.0,2.5], NoEvent, NoEvent, NoEvent,	-- 2.5 s
      Event [1.0,1.5,2.0,2.5,3.0], NoEvent, NoEvent, NoEvent,	-- 3.0 s
      Event [1.5,2.0,2.5,3.0,3.5], NoEvent, NoEvent, NoEvent,	-- 3.5 s
      Event [2.0,2.5,3.0,3.5,4.0], NoEvent, NoEvent, NoEvent,	-- 4.0 s
      Event [2.5,3.0,3.5,4.0,4.5], NoEvent, NoEvent, NoEvent,	-- 4.5 s
      Event [3.0,3.5,4.0,4.5,5.0], NoEvent, NoEvent, NoEvent,	-- 5.0 s
      Event [3.5,4.0,4.5,5.0,5.5], NoEvent, NoEvent, NoEvent,	-- 5.5 s
      Event [4.0,4.5,5.0,5.5,6.0], NoEvent			-- 6.0 s
    ]


{-
-- Not robust
utils_t16 = take 50 (embed (time >>> sampleWindow 5 0.5) input)
    where
        input = ((), [(dt, Just ()) | dt <- dts])

        dts = replicate 15 0.1
              ++ [1.0, 1.0]
              ++ replicate 15 0.1
              ++ [2.0]
              ++ replicate 10 0.1

utils_t16r =
    [ NoEvent, NoEvent,          NoEvent, NoEvent, NoEvent,		-- 0.0
      NoEvent, Event [0.6],      NoEvent, NoEvent, NoEvent,		-- 0.5
      NoEvent, Event [0.6, 1.1], NoEvent, NoEvent, NoEvent,		-- 1.0
      NoEvent,								-- 1.5
      Event [0.6,1.1,2.5,2.5,2.5],               			-- 2.5
      Event [2.5,2.5,2.5,3.5,3.5], NoEvent, NoEvent, NoEvent, NoEvent,	-- 3.5
      NoEvent, Event [2.5,2.5,3.5,3.5,4.1], NoEvent, NoEvent, NoEvent,  -- 4.0
      NoEvent, Event [2.5,3.5,3.5,4.1,4.6], NoEvent, NoEvent, NoEvent,	-- 4.5
      NoEvent,								-- 5.0
      Event [7.0,7.0,7.0,7.0,7.0], NoEvent, NoEvent, NoEvent, NoEvent,	-- 7.0
      NoEvent, Event [7.0,7.0,7.0,7.0,7.6], NoEvent, NoEvent, NoEvent,	-- 7.5
      NoEvent								-- 8.0
    ]
-}

utils_t16 = take 50 (embed (time >>> sampleWindow 5 0.4999) input)
    where
        input = ((), [(dt, Just ()) | dt <- dts])

        dts = replicate 15 0.1
              ++ [1.0, 1.0]
              ++ replicate 15 0.1
              ++ [2.0]
              ++ replicate 10 0.1

utils_t16r =
    [ NoEvent,          NoEvent, NoEvent, NoEvent, NoEvent,		-- 0.0
      Event [0.5],      NoEvent, NoEvent, NoEvent, NoEvent,		-- 0.5
      Event [0.5, 1.0], NoEvent, NoEvent, NoEvent, NoEvent,		-- 1.0
      Event [0.5, 1.0, 1.5],						-- 1.5
      Event [0.5, 1.0, 1.5, 2.5, 2.5],         				-- 2.5
      Event [1.5, 2.5, 2.5, 3.5, 3.5], NoEvent, NoEvent, NoEvent,	-- 3.5
                                                         NoEvent,
      Event [2.5, 2.5, 3.5, 3.5, 4.0], NoEvent, NoEvent, NoEvent,  	-- 4.0
      							 NoEvent,
      Event [2.5, 3.5, 3.5, 4.0, 4.5], NoEvent, NoEvent, NoEvent,	-- 4.5
							 NoEvent,
      Event [3.5, 3.5, 4.0, 4.5, 5.0],					-- 5.0
      Event [5.0, 7.0, 7.0, 7.0, 7.0], NoEvent, NoEvent, NoEvent,	-- 7.0
							 NoEvent,
      Event [7.0, 7.0, 7.0, 7.0, 7.5], NoEvent, NoEvent, NoEvent,	-- 7.5
							 NoEvent,
      Event [7.0, 7.0, 7.0, 7.5, 8.0]					-- 8.0
    ]

utils_trs =
    [ utils_t0 ~= utils_t0r,
      utils_t1 ~= utils_t1r,
      utils_t2 ~= utils_t2r,
      utils_t3 ~= utils_t3r,
      utils_t4 ~= utils_t4r,
      utils_t5 ~= utils_t5r,
      utils_t6 ~= utils_t6r,
      utils_t7 ~= utils_t7r,
      utils_t8 ~= utils_t8r,
      utils_t9 ~= utils_t9r,
      utils_t10 ~= utils_t10r,
      utils_t11 ~= utils_t11r,
      utils_t12 ~= utils_t12r,
      utils_t13 ~= utils_t13r,
      utils_t14 ~= utils_t14r,
      utils_t15 ~= utils_t15r,
      utils_t16 ~= utils_t16r
    ]

utils_tr = and utils_trs
