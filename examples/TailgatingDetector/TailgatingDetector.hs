{-# LANGUAGE Arrows #-}

{-
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         TailgatingDetector                                   *
*       Purpose:        AFRP Expressitivity Test		             *
*	Authors:	Henrik Nilsson					     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

-- Context: an autonomous flying vehicle carrying out traffic surveillance
-- through an on-board video camera.
--
-- Objective: finding a tailgater among a group of vehicles traveling along
-- a highway lane. The group is defined by the section of the highway in
-- view and thus changes dynamically as ground vehicles with non-zero
-- relative speed to the flying vehicles enters or leaves the field of
-- vision.
--
-- Simplifying assumptions:
-- * The positive x-axis of the video images is supposed to correspond to the
--   direction of travel.
-- * The flying vehicle is assumed to travel directly over and along the
--   highway lane when looking for tailgaters. The y-coordinate of the
--   highway is thus roughly 0.
-- * It is enough to consider the x-coordinate of ground vehicle positions.
--   Thus the position and velocity types are both just (signed) Double
--   for our purposes.
--
-- I find this example interesting because it makes use of TWO COLLECTION of
-- signal functions, these collections HAVE TO BE DYNAMIC by the very
-- nature of the problem, and it makes use of the the fact that CONTINUATIONS
-- ARE FIRST CLASS ENTITIES in a way which arguably also is justified
-- by the nature of the problem.

module TailgatingDetector where

import Data.List (sortBy, (\\))

import FRP.Yampa
import FRP.Yampa.Conditional
import FRP.Yampa.EventS
import FRP.Yampa.Utilities


------------------------------------------------------------------------------
-- Testing framework
------------------------------------------------------------------------------

type Position = Double	-- [m]
type Distance = Double	-- [m]
type Velocity = Double	-- [m/s]

-- We'll call any ground vehicle "car". For our purposes, a car is
-- represented by its ground position and ground velocity.
type Car = (Position, Velocity)


-- A highway is just a list of cars. In this simple setting, we assume all
-- cars are there all the time (no enter or exit ramps etc.)
type Highway = [Car]


-- Type of the Video signal. Here just an association list of cars *in view*
-- with *relative* positions.
type Video = [(Int, Car)]


-- System info, such as height and ground speed. Here, just the position.
type UAVStatus = Position


-- Various ways of making cars.
switchAfter :: Time -> SF a b -> (b -> SF a b) -> SF a b
switchAfter t sf k = switch (sf &&& after t () >>^ \(b,e) -> (b, e `tag` b)) k


mkCar1 :: Position -> Velocity -> SF a Car
mkCar1 p0 v = constant v >>> (integral >>^ (+p0)) &&& identity

mkCar2 :: Position -> Velocity -> Time -> Velocity -> SF a Car
mkCar2 p0 v0 t0 v = switchAfter t0 (mkCar1 p0 v0) (flip mkCar1 v . fst)


mkCar3 :: Position->Velocity->Time->Velocity->Time->Velocity->SF a Car
mkCar3 p0 v0 t0 v1 t1 v = switchAfter t0 (mkCar1 p0 v0) $ \(p1, _) ->
			  switchAfter t1 (mkCar1 p1 v1) $ \(p2, _) ->
                          mkCar1 p2 v


highway :: SF a Highway
highway = parB [mkCar1 (-600) 30.9,
                mkCar1 0 30,
                mkCar3 (-1000) 40 95 30 200 30.9,
		mkCar1 (-3000) 45,
                mkCar1 700 28,
                mkCar1 800 29.1]


-- The status of the UAV. For now, it's just flying at constant speed.
uavStatus :: SF a UAVStatus
uavStatus = constant 30 >>> integral


-- Tracks a car in the video stream. An event is generated when tracking is
-- lost, which we assume only happens if the car leaves the field of vision.
-- We don't concern ourselves with realistic creation of trackers.
-- The UAVStatus signal provides the current flying height and ground speed
-- which allows the perceived position to be scaled to a position in meters
-- relative to the origin directly under the flying vehicle, and the perceived
-- velocity to be transformed to ground velocity.
type CarTracker = SF (Video, UAVStatus) (Car, Event ())

range = 500

-- Creation of video stream subject to field of view and car trackers
-- as cars enters the field of view.
mkVideoAndTrackers :: SF (Highway, UAVStatus) (Video, Event CarTracker)
mkVideoAndTrackers = arr mkVideo >>> identity &&& carEntry
    where
	mkVideo :: (Highway, Position) -> Video
	mkVideo (cars, p_uav) =
            [ (i, (p_rel, v))
            | (i, (p, v)) <- zip [0..] cars,
              let p_rel = p - p_uav, abs p_rel <= range]

	carEntry :: SF Video (Event CarTracker)
	carEntry = edgeBy newCar []
	    where
		newCar v_prev v =
		    case (map fst v) \\ (map fst v_prev) of
			[]      -> Nothing
			(i : _) -> Just (mkCarTracker i)

	mkCarTracker :: Int -> CarTracker
	mkCarTracker i = arr (lookup i . fst)
                         >>> trackAndHold undefined
			     &&& edgeBy justToNothing (Just undefined)
	    where
		justToNothing Nothing  Nothing  = Nothing
		justToNothing Nothing  (Just _) = Nothing
		justToNothing (Just _) (Just _) = Nothing
		justToNothing (Just _) Nothing  = Just ()


videoAndTrackers :: SF a (Video, Event CarTracker)
videoAndTrackers = highway &&& uavStatus >>> mkVideoAndTrackers


smplFreq = 2.0
smplPer = 1/smplFreq


------------------------------------------------------------------------------
-- Tailgating detector
------------------------------------------------------------------------------

-- Looks at the positions of two cars and determines if the first is
-- tailgating the second. Tailgating is assumed to have occurred if:
-- * the first car is behind the second;
-- * the absolute speed of the first car is greater than 5 m/s;
-- * the relative speed of the cars is within 20 % of the absolute speed;
-- * the first car is no more than 5 s behind the second; and
-- * after 30 s, the average distance between the cars normalized by
--   the absolute speed is less than a second.

tailgating :: SF (Car, Car) (Event ())
tailgating = provided follow tooClose never
    where
	follow ((p1, v1), (p2, v2)) = p1 < p2
                                      && v1 > 5.0
                                      && abs ((v2 - v1)/v1) < 0.2
                                      && (p2 - p1) / v1 < 5.0

	-- Under the assumption that car c1 is following car c2, generate an
        -- event if car1 has been too close to car2 on average during the
	-- last 30 s.
	tooClose :: SF (Car, Car) (Event ())
	tooClose = proc (c1, c2) -> do
	    ead <- recur (snapAfter 30 <<< avgDist) -< (c1, c2)
	    returnA -< (filterE (<1.0) ead) `tag` ()

        avgDist = proc ((p1, v1), (p2, v2)) -> do
	    let nd = (p2 - p1) / v1
	    ind <- integral  -< nd
	    t   <- localTime -< ()
            returnA -< if t > 0 then ind / t else nd


------------------------------------------------------------------------------
-- Multi-Car tracker
------------------------------------------------------------------------------

-- Auxiliary definitions

type Id = Int

data MCTCol a = MCTCol Id [(Id, a)]


instance Functor MCTCol where
    fmap f (MCTCol n ias) = MCTCol n [ (i, f a) | (i, a) <- ias ]


-- Tracking of individual cars in a group. The arrival of a new car is
-- signalled by an external event, which causes a new tracker to be added
-- to internal collection of car trackers. A tracker is removed as soon
-- as it looses tracking.
--
-- The output consists of the output from the individual trackers, tagged
-- with an assigned identity unique to each tracker.
--
-- I'M GIVING UP ON THIS BIT FOR NOW
-- The external identity event signals that the car being tracked by the
-- tracker tagged by the identity carried by the event is guilty of
-- tailgating. This causes an event carrying the *continuation* of the
-- corresponding tracker to be generated, e.g. allowing the overall
-- controll system to focus on follwing that particular car without first
-- having to start a new tracker (risking misidentification).

mct :: SF (Video, UAVStatus, Event CarTracker) [(Id, Car)]
mct = pSwitch route cts_init addOrDelCTs (\cts' f -> mctAux (f cts'))
      >>^ getCars
    where
	mctAux cts = pSwitch route
			     cts
			     (noEvent --> addOrDelCTs)
			     (\cts' f -> mctAux (f cts'))

	route (v, s, _) = fmap (\ct -> ((v, s), ct))

	-- addOrDelCTs :: SF _ (Event (MCTCol CarTracker -> MCTCol carTracker))
	addOrDelCTs = proc ((_, _, ect), ces) -> do
	    let eAdd = fmap addCT ect
            let eDel = fmap delCTs (catEvents (getEvents ces))
            returnA -< mergeBy (.) eAdd eDel

	cts_init :: MCTCol CarTracker
	cts_init = MCTCol 0 []

	addCT :: CarTracker -> MCTCol CarTracker -> MCTCol CarTracker
	addCT ct (MCTCol n icts) = MCTCol (n+1) ((n, ct) : icts)

	delCTs :: [Id] -> MCTCol CarTracker -> MCTCol CarTracker
	delCTs is (MCTCol n icts) =
            MCTCol n (filter (flip notElem is . fst) icts)

	getCars :: MCTCol (Car, Event ()) -> [(Id, Car)]
	getCars (MCTCol _ ices) = [(i, c) | (i, (c, _)) <- ices ]

	getEvents :: MCTCol (Car, Event ()) -> [Event Id]
	getEvents (MCTCol _ ices) = [e `tag` i | (i,(_,e)) <- ices]


------------------------------------------------------------------------------
-- Multi tailgating detector
------------------------------------------------------------------------------

-- Auxiliary definitions

newtype MTGDCol a = MTGDCol [((Id,Id), a)]


instance Functor MTGDCol where
    fmap f (MTGDCol iias) = MTGDCol [ (ii, f a) | (ii, a) <- iias ]


-- Run tailgating above for each pair of tracked cars. A structural change
-- to the list of tracked cars is signalled by an event, at which point
-- the signal function will figure which old tailgating detectors that have
-- to be removed and which new that have to be started based on an initial
-- sample of the new configuration. An event carrying the identity of
-- a tailgater and the one being tailgated is generated when one of the
-- tailgating signal functions generates an event.

mtgd :: SF [(Id, Car)] (Event [(Id, Id)])
mtgd = proc ics -> do
    let ics' = sortBy relPos ics
    eno  <- newOrder -< ics'
    etgs <- rpSwitch route (MTGDCol []) -< (ics', fmap updateTGDs eno)
    returnA -< tailgaters etgs
    where
	route ics (MTGDCol iitgs) = MTGDCol $
	    let cs = map snd ics
	    in
	        [ (ii, (cc, tg))
		| (cc, (ii, tg)) <- zip (zip cs (tail cs)) iitgs ]

	relPos (_, (p1, _)) (_, (p2, _)) = compare p1 p2

	newOrder :: SF [(Id, Car)] (Event [Id])
	newOrder = edgeBy (\ics ics' -> if sameOrder ics ics' then
					    Nothing
					else
					    Just (map fst ics'))
			  []
	    where
		sameOrder [] [] = True
		sameOrder [] _  = False
		sameOrder _  [] = False
		sameOrder ((i,_):ics) ((i',_):ics')
		    | i == i'   = sameOrder ics ics' 
		    | otherwise = False

	updateTGDs is (MTGDCol iitgs) = MTGDCol $
	    [ (ii, maybe tailgating id (lookup ii iitgs))
	    | ii <- zip is (tail is) ]

	tailgaters :: MTGDCol (Event ()) -> Event [(Id, Id)]
	tailgaters (MTGDCol iies) = catEvents [ e `tag` ii | (ii, e) <- iies ]


-- Finally, we can tie the individaul pieces together into a signal
-- function which finds tailgaters:

findTailgaters ::
    SF (Video, UAVStatus, Event CarTracker) ([(Id, Car)], Event [(Id, Id)])
findTailgaters = proc (v, s, ect) -> do
    ics  <- mct  -< (v, s, ect)
    etgs <- mtgd -< ics
    returnA -< (ics, etgs)
