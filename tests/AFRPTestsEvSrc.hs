{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsEvSrc.hs,v 1.3 2003/12/19 15:32:22 henrik Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsEvSrc					     *
*       Purpose:        Test cases for event sources			     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsEvSrc (evsrc_trs, evsrc_tr) where

import FRP.Yampa
import FRP.Yampa.Internals (Event(NoEvent, Event))

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for basic event sources and stateful event suppression
------------------------------------------------------------------------------

evsrc_t0 :: [Event ()]
evsrc_t0 = testSF1 never

evsrc_t0r =
    [NoEvent, NoEvent, NoEvent, NoEvent,	-- 0.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 2.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 3.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 4.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t1 :: [Event Int]
evsrc_t1 = testSF1 (now 42)

evsrc_t1r :: [Event Int]
evsrc_t1r =
    [Event 42, NoEvent, NoEvent, NoEvent,	-- 0.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 2.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 3.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 4.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t2 :: [Event Int]
evsrc_t2 = testSF1 (after 0.0 42)
evsrc_t2r :: [Event Int]
evsrc_t2r =
    [Event 42, NoEvent, NoEvent, NoEvent,	-- 0.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 2.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 3.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 4.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t3 :: [Event Int]
evsrc_t3 = testSF1 (after 3.0 42)

evsrc_t3r :: [Event Int]
evsrc_t3r =
    [NoEvent,  NoEvent, NoEvent, NoEvent,	-- 0.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 2.0 s
     Event 42, NoEvent, NoEvent, NoEvent,	-- 3.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 4.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t4 :: [Event Int]
evsrc_t4 = testSF1 (after 3.01 42)

evsrc_t4r :: [Event Int]
evsrc_t4r =
    [NoEvent, NoEvent,  NoEvent, NoEvent,	-- 0.0 s
     NoEvent, NoEvent,  NoEvent, NoEvent,	-- 1.0 s
     NoEvent, NoEvent,  NoEvent, NoEvent,	-- 2.0 s
     NoEvent, Event 42, NoEvent, NoEvent,	-- 3.0 s
     NoEvent, NoEvent,  NoEvent, NoEvent,	-- 4.0 s
     NoEvent, NoEvent,  NoEvent, NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t5 :: [Event Int]
evsrc_t5 = testSF1 (repeatedly 0.795 42)

evsrc_t5r :: [Event Int]
evsrc_t5r =
    [NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     Event 42, NoEvent,  NoEvent,  Event 42,	-- 1.0 s
     NoEvent,  NoEvent,  Event 42, NoEvent,	-- 2.0 s
     NoEvent,  Event 42, NoEvent,  NoEvent,	-- 3.0 s
     Event 42, NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     Event 42, NoEvent,  NoEvent,  Event 42,	-- 5.0 s
     NoEvent]

evsrc_t6 :: [Event Int]
evsrc_t6 = testSF1 (repeatedly 0.29999 42)

evsrc_t6r :: [Event Int]
evsrc_t6r =
    [NoEvent,  NoEvent,  Event 42, Event 42,	-- 0.0 s
     Event 42, Event 42, Event 42, NoEvent,	-- 1.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 2.0 s
     Event 42, NoEvent,  Event 42, Event 42,	-- 3.0 s
     Event 42, Event 42, Event 42, NoEvent,	-- 4.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 5.0 s
     Event 42]

evsrc_t7 :: [Event Int]
evsrc_t7 = testSF1 (repeatedly 0.24 42)

evsrc_t7r :: [Event Int]
evsrc_t7r =
    [NoEvent,  Event 42, Event 42, Event 42,	-- 0.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 1.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 2.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 3.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 4.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 5.0 s
     Event 42]


evsrc_t8 :: [Event Int]
evsrc_t8 = testSF1 (afterEach [(0.00, 1), (0.00, 2), (0.01, 3), (0.23, 4),
                               (0.02, 5), (0.75, 6), (0.10, 7), (0.10, 8),
			       (0.10, 9), (2.00, 10)])

evsrc_t8r :: [Event Int]
evsrc_t8r =
    [Event 1,  Event 3,  Event 5,  NoEvent,	-- 0.0 s
     NoEvent,  Event 6,  Event 9,  NoEvent,	-- 1.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 2.0 s
     NoEvent,  NoEvent,  Event 10, NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t9 :: [Event Int]
evsrc_t9 = testSF1 (afterEach [(2.03, 0),
			       (0.00, 1), (0.00, 2), (0.01, 3), (0.23, 4),
                               (0.02, 5), (0.75, 6), (0.10, 7), (0.10, 8),
			       (0.10, 9), (2.00, 10), (0.00, 11), (0.00, 12)])

evsrc_t9r :: [Event Int]
evsrc_t9r =
    [NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 1.0 s
     NoEvent,  Event 0,  Event 4,  NoEvent,	-- 2.0 s
     NoEvent,  Event 6,  Event 9,  NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,  Event 10, NoEvent,	-- 5.0 s
     NoEvent]



evsrc_t10 :: [Event [Int]]
evsrc_t10 = testSF1 (afterEachCat [(0.00, 1), (0.00, 2), (0.01, 3), (0.23, 4),
                                   (0.02, 5), (0.75, 6), (0.10, 7), (0.10, 8),
			           (0.10, 9), (2.00, 10)])

evsrc_t10r :: [Event [Int]]
evsrc_t10r =
    [Event [1,2],  Event [3,4],    Event [5],  NoEvent,	-- 0.0 s
     NoEvent,      Event [6,7,8],  Event [9],  NoEvent,	-- 1.0 s
     NoEvent,      NoEvent,        NoEvent,    NoEvent,	-- 2.0 s
     NoEvent,      NoEvent,        Event [10], NoEvent,	-- 3.0 s
     NoEvent,      NoEvent,        NoEvent,    NoEvent,	-- 4.0 s
     NoEvent,      NoEvent,        NoEvent,    NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t11 :: [Event [Int]]
evsrc_t11 = testSF1 (afterEachCat [(2.03, 0),
			           (0.00, 1), (0.00, 2), (0.01, 3), (0.23, 4),
                                   (0.02, 5), (0.75, 6), (0.10, 7), (0.10, 8),
			           (0.10, 9), (2.00, 10)])

evsrc_t11r :: [Event [Int]]
evsrc_t11r =
    [NoEvent,  NoEvent,         NoEvent,     NoEvent,	-- 0.0 s
     NoEvent,  NoEvent,         NoEvent,     NoEvent,	-- 1.0 s
     NoEvent,  Event [0,1,2,3], Event [4,5], NoEvent,	-- 2.0 s
     NoEvent,  Event [6,7,8],   Event [9],   NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,         NoEvent,     NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,         Event [10],  NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t12 :: [Event ()]
evsrc_t12 = testSF1 (localTime >>> arr (>=0) >>> edge)

evsrc_t12r = 
    [NoEvent, NoEvent, NoEvent, NoEvent,	-- 0.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 2.0 s
     NoEvent, NoEvent, NoEvent,	NoEvent,	-- 3.0 s
     NoEvent, NoEvent, NoEvent,	NoEvent,	-- 4.0 s
     NoEvent, NoEvent, NoEvent,	NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t13 :: [Event ()]
evsrc_t13 = testSF1 (localTime >>> arr (>=4.26) >>> edge)

evsrc_t13r =
    [NoEvent, NoEvent, NoEvent,  NoEvent,	-- 0.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 1.0 s
     NoEvent, NoEvent, NoEvent,  NoEvent,	-- 2.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 3.0 s
     NoEvent, NoEvent, Event (), NoEvent,	-- 4.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 5.0 s
     NoEvent]


-- Raising edge detector.
evsrc_isEdge False False = Nothing
evsrc_isEdge False True  = Just ()
evsrc_isEdge True  True  = Nothing
evsrc_isEdge True  False = Nothing


evsrc_t14 :: [Event ()]
evsrc_t14 = testSF1 (localTime >>> arr (>=0) >>> edgeBy evsrc_isEdge False)

evsrc_t14r = 
    [Event (), NoEvent, NoEvent, NoEvent,	-- 0.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 2.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 3.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 4.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 5.0 s
     NoEvent]

evsrc_t15 :: [Event ()]
evsrc_t15 = testSF1 (localTime >>> arr (>=4.26) >>> edgeBy evsrc_isEdge False)

evsrc_t15r =
    [NoEvent, NoEvent, NoEvent,  NoEvent,	-- 0.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 1.0 s
     NoEvent, NoEvent, NoEvent,  NoEvent,	-- 2.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 3.0 s
     NoEvent, NoEvent, Event (), NoEvent,	-- 4.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 5.0 s
     NoEvent]

-- Raising and falling edge detector.
evsrc_isEdge2 False False = Nothing
evsrc_isEdge2 False True  = Just True
evsrc_isEdge2 True  True  = Nothing
evsrc_isEdge2 True  False = Just False

evsrc_t16 :: [Event Bool]
evsrc_t16 = testSF1 (localTime
                    >>> arr (\t -> t >=2.01 && t <= 4.51)
		    >>> edgeBy evsrc_isEdge2 True)

evsrc_t16r =
    [Event False, NoEvent,    NoEvent, NoEvent,		-- 0.0 s
     NoEvent,     NoEvent,    NoEvent, NoEvent,		-- 1.0 s
     NoEvent,     Event True, NoEvent, NoEvent,		-- 2.0 s
     NoEvent,     NoEvent,    NoEvent, NoEvent,		-- 3.0 s
     NoEvent,     NoEvent,    NoEvent, Event False,	-- 4.0 s
     NoEvent,     NoEvent,    NoEvent, NoEvent,		-- 5.0 s
     NoEvent]

evsrc_t17 :: [Event Int]
evsrc_t17 = testSF1 (now 17 &&& repeatedly 0.795 42
		     >>> arr (uncurry merge)
		     >>> notYet)

evsrc_t17r :: [Event Int]
evsrc_t17r =
    [NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     Event 42, NoEvent,  NoEvent,  Event 42,	-- 1.0 s
     NoEvent,  NoEvent,  Event 42, NoEvent,	-- 2.0 s
     NoEvent,  Event 42, NoEvent,  NoEvent,	-- 3.0 s
     Event 42, NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     Event 42, NoEvent,  NoEvent,  Event 42,	-- 5.0 s
     NoEvent]


evsrc_t18 :: [Event Int]
evsrc_t18 = testSF1 (now 42 >>> once)

evsrc_t18r :: [Event Int]
evsrc_t18r =
    [Event 42, NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 1.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 2.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t19 :: [Event Int]
evsrc_t19 = testSF1 (repeatedly 0.8 42 >>> once)

evsrc_t19r :: [Event Int]
evsrc_t19r =
    [NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     Event 42, NoEvent,  NoEvent,  NoEvent,	-- 1.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 2.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t20 :: [Event Int]
evsrc_t20 = testSF1 (now 42 >>> takeEvents 0)

evsrc_t20r :: [Event Int]
evsrc_t20r =
    [NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 1.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 2.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t21 :: [Event Int]
evsrc_t21 = testSF1 (now 42 >>> takeEvents 1)

evsrc_t21r :: [Event Int]
evsrc_t21r =
    [Event 42, NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 1.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 2.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t22 :: [Event Int]
evsrc_t22 = testSF1 (repeatedly 0.8 42 >>> takeEvents 4)

evsrc_t22r :: [Event Int]
evsrc_t22r =
    [NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     Event 42, NoEvent,  NoEvent,  Event 42,	-- 1.0 s
     NoEvent,  NoEvent,  Event 42, NoEvent,	-- 2.0 s
     NoEvent,  Event 42, NoEvent,  NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t23 :: [Event Int]
evsrc_t23 = testSF1 (repeatedly 0.2 42 >>> takeEvents 4)

evsrc_t23r :: [Event Int]
evsrc_t23r =
    [NoEvent,  Event 42, Event 42, Event 42,	-- 0.0 s
     Event 42, NoEvent,  NoEvent,  NoEvent,	-- 1.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 2.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t24 :: [Event Int]
evsrc_t24 = testSF1 (now 42 >>> dropEvents 0)

evsrc_t24r :: [Event Int]
evsrc_t24r =
    [Event 42, NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 1.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 2.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t25 :: [Event Int]
evsrc_t25 = testSF1 (now 42 >>> dropEvents 1)

evsrc_t25r :: [Event Int]
evsrc_t25r =
    [NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 1.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 2.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 5.0 s
     NoEvent]


evsrc_t26 :: [Event Int]
-- Drop 5 events to get rid of the event at 4.0 s which may or may not happen
-- exactly there. 
evsrc_t26 = testSF1 (repeatedly 0.8 42 >>> dropEvents 5)

evsrc_t26r :: [Event Int]
evsrc_t26r =
    [NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 1.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 2.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 3.0 s
     NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 4.0 s
     Event 42, NoEvent,  NoEvent,  Event 42,	-- 5.0 s
     NoEvent]


evsrc_t27 :: [Event Int]
evsrc_t27 = testSF1 (repeatedly 0.2 42 >>> dropEvents 4)

evsrc_t27r :: [Event Int]
evsrc_t27r =
    [NoEvent,  NoEvent,  NoEvent,  NoEvent,	-- 0.0 s
     NoEvent,  Event 42, Event 42, Event 42,	-- 1.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 2.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 3.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 4.0 s
     Event 42, Event 42, Event 42, Event 42,	-- 5.0 s
     Event 42]



evsrc_t28 :: [(Event Int, Event Int)]
evsrc_t28 = embed (repeatedly 0.5 ()
                   >>> accumBy (\n _ -> n + 1) 0
                   >>> identity &&& delayEvent 2.0)
                  (deltaEncode 0.125 (replicate 50 ()))

evsrc_t28r =
    [ (NoEvent,NoEvent),  (NoEvent,NoEvent),	-- 0.0 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 1,NoEvent),  (NoEvent,NoEvent),	-- 0.5 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 2,NoEvent),  (NoEvent,NoEvent),	-- 1.0 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 3,NoEvent),  (NoEvent,NoEvent),	-- 1.5 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 4,NoEvent),  (NoEvent,NoEvent),	-- 2.0 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 5,Event 1),  (NoEvent,NoEvent),	-- 2.5 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 6,Event 2),  (NoEvent,NoEvent),	-- 3.0 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 7,Event 3),  (NoEvent,NoEvent),	-- 3.5 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 8,Event 4),  (NoEvent,NoEvent),	-- 4.0 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 9,Event 5),  (NoEvent,NoEvent),	-- 4.5 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 10,Event 6), (NoEvent,NoEvent),	-- 5.0 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 11,Event 7), (NoEvent,NoEvent),	-- 5.5 s
      (NoEvent,NoEvent),  (NoEvent,NoEvent),
      (Event 12,Event 8), (NoEvent,NoEvent)	-- 6.0 s
    ]

evsrc_t29 :: [Event [Double]]
evsrc_t29 = embed (time &&& repeatedly 0.5001 ()
                   >>> arr (\(t,e) -> e `tag` t)
                   >>> delayEventCat 3.0)
                  input
    where
        dts   = replicate 40 0.1 ++ [2.0] ++ replicate 40 0.1
	input = ((), [(dt, Just ()) | dt <- dts]) 

{- Resulting input to the delay for reference:
[ NoEvent,   NoEvent,   NoEvent, NoEvent, NoEvent,	-- 0.0 s 
  NoEvent,   Event 0.6, NoEvent, NoEvent, NoEvent,	-- 0.5 s 
  NoEvent,   Event 1.1, NoEvent, NoEvent, NoEvent,	-- 1.0 s 
  NoEvent,   Event 1.6, NoEvent, NoEvent, NoEvent,	-- 1.5 s 
  NoEvent,   Event 2.1, NoEvent, NoEvent, NoEvent,	-- 2.0 s 
  NoEvent,   Event 2.6, NoEvent, NoEvent, NoEvent,	-- 2.5 s 
  NoEvent,   Event 3.1, NoEvent, NoEvent, NoEvent,	-- 3.0 s 
  NoEvent,   Event 3.6, NoEvent, NoEvent, NoEvent,	-- 3.5 s 
  NoEvent,						-- 4.0 s 
  Event 6.0, Event 6.1, NoEvent, NoEvent, NoEvent,	-- 6.0 s 
  NoEvent,   Event 6.6, NoEvent, NoEvent, NoEvent,	-- 6.5 s          
  NoEvent,   Event 7.1, NoEvent, NoEvent, NoEvent,	-- 7.0 s 
  NoEvent,   Event 7.6, NoEvent, NoEvent, NoEvent,	-- 7.5 s 
  NoEvent,   Event 8.1, NoEvent, NoEvent, NoEvent,	-- 8.0 s 
  NoEvent,   Event 8.6, NoEvent, NoEvent, NoEvent,	-- 8.5 s 
  NoEvent,   Event 9.1, NoEvent, NoEvent, NoEvent,	-- 9.0 s 
  NoEvent,   Event 9.6, NoEvent, NoEvent, NoEvent,	-- 9.5 s 
  NoEvent ]						-- 10.0 s
-}

evsrc_t29r =
    [ NoEvent, NoEvent,     NoEvent, NoEvent, NoEvent,		-- 0.0 s
      NoEvent, NoEvent,     NoEvent, NoEvent, NoEvent,		-- 0.5 s
      NoEvent, NoEvent,     NoEvent, NoEvent, NoEvent,		-- 1.0 s
      NoEvent, NoEvent,     NoEvent, NoEvent, NoEvent,		-- 1.5 s
      NoEvent, NoEvent,     NoEvent, NoEvent, NoEvent,		-- 2.0 s
      NoEvent, NoEvent,     NoEvent, NoEvent, NoEvent,		-- 2.5 s
      NoEvent, NoEvent,     NoEvent, NoEvent, NoEvent,		-- 3.0 s
      NoEvent, Event [0.6], NoEvent, NoEvent, NoEvent,		-- 3.5 s
      NoEvent,							-- 4.0 s
      Event [1.1, 1.6, 2.1, 2.6],				-- 6.0 s
          NoEvent, Event [3.1], NoEvent, NoEvent,
      NoEvent, NoEvent,     Event [3.6], NoEvent, NoEvent,	-- 6.5 s
      NoEvent, NoEvent,     NoEvent,     NoEvent, NoEvent,	-- 7.0 s
      NoEvent, NoEvent,     NoEvent,     NoEvent, NoEvent,	-- 7.5 s
      NoEvent, NoEvent,     NoEvent,     NoEvent, NoEvent,	-- 8.0 s
      NoEvent, NoEvent,     NoEvent,     NoEvent, NoEvent,	-- 8.5 s
      NoEvent, Event [6.0], Event [6.1], NoEvent, NoEvent,	-- 9.0 s
      NoEvent, NoEvent,     Event [6.6], NoEvent, NoEvent,	-- 9.5 s
      NoEvent							-- 10.0 s
    ]

-- "delayEvent" in a feedback loop. Should work like "repeatedly".
evsrc_t30 :: [(Event ())]
evsrc_t30 = embed (now ()
                   >>> (loop $
			    arr (uncurry lMerge)
                            >>> delayEvent 1.0
                            >>> arr dup))
                  (deltaEncode 0.125 (replicate 50 ()))


evsrc_t30r :: [(Event ())]
evsrc_t30r =
    [ NoEvent,  NoEvent, NoEvent, NoEvent,	-- 0.0 s
      NoEvent,  NoEvent, NoEvent, NoEvent,	-- 0.5 s
      Event (), NoEvent, NoEvent, NoEvent, 	-- 1.0 s
      NoEvent,  NoEvent, NoEvent, NoEvent,	-- 1.5 s
      Event (), NoEvent, NoEvent, NoEvent,	-- 2.0 s
      NoEvent,  NoEvent, NoEvent, NoEvent,	-- 2.5 s
      Event (), NoEvent, NoEvent, NoEvent,	-- 3.0 s
      NoEvent,  NoEvent, NoEvent, NoEvent,	-- 3.5 s
      Event (), NoEvent, NoEvent, NoEvent,	-- 4.0 s
      NoEvent,  NoEvent, NoEvent, NoEvent,	-- 4.5 s
      Event (), NoEvent, NoEvent, NoEvent,	-- 5.0 s
      NoEvent,  NoEvent, NoEvent, NoEvent,	-- 5.5 s
      Event (), NoEvent				-- 6.0 s
    ]


evsrc_trs =
    [ evsrc_t0 ~= evsrc_t0r,
      evsrc_t1 ~= evsrc_t1r,
      evsrc_t2 ~= evsrc_t2r,
      evsrc_t3 ~= evsrc_t3r,
      evsrc_t4 ~= evsrc_t4r,
      evsrc_t5 ~= evsrc_t5r,
      evsrc_t6 ~= evsrc_t6r,
      evsrc_t7 ~= evsrc_t7r,
      evsrc_t8 ~= evsrc_t8r,
      evsrc_t9 ~= evsrc_t9r,
      evsrc_t10 ~= evsrc_t10r,
      evsrc_t11 ~= evsrc_t11r,
      evsrc_t12 ~= evsrc_t12r,
      evsrc_t13 ~= evsrc_t13r,
      evsrc_t14 ~= evsrc_t14r,
      evsrc_t15 ~= evsrc_t15r,
      evsrc_t16 ~= evsrc_t16r,
      evsrc_t17 ~= evsrc_t17r,
      evsrc_t18 ~= evsrc_t18r,
      evsrc_t19 ~= evsrc_t19r,
      evsrc_t20 ~= evsrc_t20r,
      evsrc_t21 ~= evsrc_t21r,
      evsrc_t22 ~= evsrc_t22r,
      evsrc_t23 ~= evsrc_t23r,
      evsrc_t24 ~= evsrc_t24r,
      evsrc_t25 ~= evsrc_t25r,
      evsrc_t26 ~= evsrc_t26r,
      evsrc_t27 ~= evsrc_t27r,
      evsrc_t28 ~= evsrc_t28r,
      evsrc_t29 ~= evsrc_t29r,
      evsrc_t30 ~= evsrc_t30r
    ]

evsrc_tr = and evsrc_trs
