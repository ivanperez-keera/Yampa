{-# LANGUAGE Arrows #-}

{-
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Example:        Test TG                                              *
*       Purpose:        Testing of the tailgating detector.	             *
*	Authors:	Henrik Nilsson					     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module Main where

import Data.List (sortBy)

import FRP.Yampa
import FRP.Yampa.Utilities
import FRP.Yampa.Internals	-- Just for testing purposes.

import TailgatingDetector


-- Looks for interesting events in the video stream (cars entering,
-- leaving, overtaking) in the interval [0, t].
testVideo :: Time -> [(Time, Event Video)]
testVideo t_max = filter (isEvent . snd) $
                  takeWhile (\(t, _) -> t <= t_max) $
                  embed (localTime &&& (videoAndTrackers >>^ fst)
			 >>> filterVideo)
	          (deltaEncode smplPer (repeat ()))
    where
	filterVideo = second (edgeBy change [])
	    where
		change v_prev v =
		    if (map fst (sortBy comparePos v_prev))
                       /= (map fst (sortBy comparePos v)) then
			Just v
		    else
			Nothing 

	comparePos (_, (p1, _)) (_, (p2, _)) = compare p1 p2


ppTestVideo t = mapM_ (putStrLn . show) (testVideo t)


testTailgating t_max = filter (isEvent . snd) $
                       takeWhile (\(t, _) -> t <= t_max) $
                       embed (localTime
			      &&& (mkCar3 (-1000) 40 95 30 200 30.9
				   &&& mkCar1 0 30
				   >>> tailgating))
	               (deltaEncode smplPer (repeat ()))


testMCT :: Time -> [(Time, Event [(Id, Car)])]
testMCT t_max = filter (isEvent . snd) $
                takeWhile (\(t, _) -> t <= t_max) $
                embed (localTime
                       &&& (uavStatus
                            >>> (highway &&& identity >>> mkVideoAndTrackers)
                                &&& identity
                            >>> arr (\((v, ect), s) -> (v, s, ect))
                            >>> mct)
		       >>> filterMCTOutput)
	        (deltaEncode smplPer (repeat ()))
    where
	filterMCTOutput = second (edgeBy change [])
	    where
		change v_prev v =
		    if (map fst (sortBy comparePos v_prev))
                       /= (map fst (sortBy comparePos v)) then
			Just v
		    else
			Nothing 

	comparePos (_, (p1, _)) (_, (p2, _)) = compare p1 p2


ppTestMCT t = mapM_ (putStrLn . show) (testMCT t)


testMTGD :: Time -> [(Time, (Event [(Id,Id)], [(Id, Car)]))]
testMTGD t_max = filter (isEvent . fst . snd) $
                 takeWhile (\(t, _) -> t <= t_max) $
                 embed (localTime
                        &&& (proc _ -> do s           <- uavStatus          -< ()
                                          h           <- highway            -< ()
                                          (v, ect)    <- mkVideoAndTrackers -< (h, s)
                                          (ics, etgs) <- findTailgaters     -< (v,s,ect) 
                                          etgs        <- mtgd               -< ics
                                          returnA     -< (etgs, ics)))
                       (deltaEncode smplPer (repeat ()))

ppTestMTGD t = mapM_ (putStrLn . show) (testMTGD t)


-- We could read the car specification from standard input.
main = ppTestMTGD 2000
