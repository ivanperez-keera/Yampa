{-# LANGUAGE GADTs, Rank2Types, CPP #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
-- 
--
-- Domain-specific language embedded in Haskell for programming hybrid (mixed
-- discrete-time and continuous-time) systems. Yampa is based on the concepts
-- of Functional Reactive Programming (FRP) and is structured using arrow
-- combinators.
--
-- You can find examples, tutorials and documentation on Yampa here:
--
-- <www.haskell.org/haskellwiki/Yampa>
--
-- Structuring a hybrid system in Yampa is done based on two main concepts:
--
-- * Signal Functions: 'SF'. Yampa is based on the concept of Signal Functions,
-- which are functions from a typed input signal to a typed output signal.
-- Conceptually, signals are functions from Time to Value, where time are the
-- real numbers and, computationally, a very dense approximation (Double) is
-- used.
--
-- * Events: 'Event'. Values that may or may not occur (and would probably
-- occur rarely). It is often used for incoming network messages, mouse
-- clicks, etc. Events are used as values carried by signals.
--
-- A complete Yampa system is defined as one Signal Function from some
-- type @a@ to a type @b@. The execution of this signal transformer
-- with specific input can be accomplished by means of two functions:
-- 'reactimate' (which needs an initialization action,
-- an input sensing action and an actuation/consumer action and executes
-- until explicitly stopped), and 'react' (which executes only one cycle).
-- 
-- Apart from using normal functions and arrow syntax to define 'SF's, you
-- can also use several combinators. See [<#g:4>] for basic signals combinators,
-- [<#g:11>] for ways of switching from one signal transformation to another,
-- and [<#g:16>] for ways of transforming Event-carrying signals into continuous
-- signals, [<#g:19>] for ways of delaying signals, and [<#g:21>] for ways to
-- feed a signal back to the same signal transformer.
--
-- Ways to define Event-carrying signals are given in [<#g:7>], and
-- "FRP.Yampa.Event" defines events and event-manipulation functions.
--
-- Finally, see [<#g:26>] for sources of randomness (useful in games).
--
-- CHANGELOG:
--
-- * Adds (most) documentation.
--
-- * New version using GADTs.
--
-- ToDo:
--
-- * Specialize def. of repeatedly. Could have an impact on invaders.
--
-- * New defs for accs using SFAcc
--
-- * Make sure opt worked: e.g.
--
--   >     repeatedly >>> count >>> arr (fmap sqr)
--
-- * Introduce SFAccHld.
--
-- * See if possible to unify AccHld wity Acc??? They are so close.
--
-- * Introduce SScan. BUT KEEP IN MIND: Most if not all opts would
--   have been possible without GADTs???
--
-- * Look into pairs. At least pairing of SScan ought to be interesting.
--
-- * Would be nice if we could get rid of first & second with impunity
--   thanks to Id optimizations. That's a clear win, with or without
--   an explicit pair combinator.
--
-- * delayEventCat is a bit complicated ...
--
--
-- Random ideas:
--
-- * What if one used rules to optimize
--   - (arr :: SF a ()) to (constant ())
--   - (arr :: SF a a) to identity
--   But inspection of invader source code seem to indicate that
--   these are not very common cases at all.
--
-- * It would be nice if it was possible to come up with opt. rules
--   that are invariant of how signal function expressions are
--   parenthesized. Right now, we have e.g.
--       arr f >>> (constant c >>> sf)
--   being optimized to
--       cpAuxA1 f (cpAuxC1 c sf)
--   whereas it clearly should be possible to optimize to just
--       cpAuxC1 c sf
--   What if we didn't use SF' but
--      SFComp :: <tfun> -> SF' a b -> SF' b c -> SF' a c
--   ???
--
-- * The transition function would still be optimized in (pretty much)
--   the current way, but it would still be possible to look "inside"
--   composed signal functions for lost optimization opts.
--   Seems to me this could be done without too much extra effort/no dupl.
--   work.
--   E.g. new cpAux, the general case:
--
-- @
--      cpAux sf1 sf2 = SFComp tf sf1 sf2
--          where
--              tf dt a = (cpAux sf1' sf2', c)
--                  where
--                      (sf1', b) = (sfTF' sf1) dt a
--                      (sf2', c) = (sfTF' sf2) dt b
-- @
--
-- * The ONLY change was changing the constructor from SF' to SFComp and
--   adding sf1 and sf2 to the constructor app.!
--
-- * An optimized case:
--     cpAuxC1 b sf1 sf2               = SFComp tf sf1 sf2
--   So cpAuxC1 gets an extra arg, and we change the constructor.
--   But how to exploit without writing 1000s of rules???
--   Maybe define predicates on SFComp to see if the first or second
--   sf are "interesting", and if so, make "reassociate" and make a
--   recursive call? E.g. we're in the arr case, and the first sf is another
--   arr, so we'd like to combine the two.
--
-- * It would also be intersting, then, to know when to STOP playing this
--   game, due to the overhead involved.
--
-- * Why don't we have a "SWITCH" constructor that indicates that the
--   structure will change, and thus that it is worthwile to keep
--   looking for opt. opportunities, whereas a plain "SF'" would
--   indicate that things NEVER are going to change, and thus we can just
--   as well give up?
-----------------------------------------------------------------------------------------

module FRP.Yampa.EventS (
-- * Discrete to continuous-time signal functions
-- ** Wave-form generation
    old_hold,		-- :: a -> SF (Event a) a
    hold,		-- :: a -> SF (Event a) a
    dHold,		-- :: a -> SF (Event a) a
    trackAndHold,	-- :: a -> SF (Maybe a) a

-- ** Accumulators
    accum,		-- :: a -> SF (Event (a -> a)) (Event a)
    accumHold,		-- :: a -> SF (Event (a -> a)) a
    dAccumHold,		-- :: a -> SF (Event (a -> a)) a
    accumBy,		-- :: (b -> a -> b) -> b -> SF (Event a) (Event b)
    accumHoldBy,	-- :: (b -> a -> b) -> b -> SF (Event a) b
    dAccumHoldBy,	-- :: (b -> a -> b) -> b -> SF (Event a) b
    accumFilter,	-- :: (c -> a -> (c, Maybe b)) -> c
			--    -> SF (Event a) (Event b)
    old_accum,		-- :: a -> SF (Event (a -> a)) (Event a)
    old_accumBy,	-- :: (b -> a -> b) -> b -> SF (Event a) (Event b)
    old_accumFilter,	-- :: (c -> a -> (c, Maybe b)) -> c


-- * Events
-- ** Basic event sources
    never, 		-- :: SF a (Event b)
    now,		-- :: b -> SF a (Event b)
    after,		-- :: Time -> b -> SF a (Event b)
    repeatedly,		-- :: Time -> b -> SF a (Event b)
    afterEach,		-- :: [(Time,b)] -> SF a (Event b)
    afterEachCat,       -- :: [(Time,b)] -> SF a (Event [b])
    delayEvent,		-- :: Time -> SF (Event a) (Event a)
    delayEventCat,	-- :: Time -> SF (Event a) (Event [a])
    edge,		-- :: SF Bool (Event ())
    iEdge,		-- :: Bool -> SF Bool (Event ())
    edgeTag,		-- :: a -> SF Bool (Event a)
    edgeJust,		-- :: SF (Maybe a) (Event a)
    edgeBy,		-- :: (a -> a -> Maybe b) -> a -> SF a (Event b)

-- ** Stateful event suppression
    notYet,		-- :: SF (Event a) (Event a)
    once,		-- :: SF (Event a) (Event a)
    takeEvents,		-- :: Int -> SF (Event a) (Event a)
    dropEvents,		-- :: Int -> SF (Event a) (Event a)

-- ** Pointwise functions on events
    noEvent,		-- :: Event a
    noEventFst,		-- :: (Event a, b) -> (Event c, b)
    noEventSnd,		-- :: (a, Event b) -> (a, Event c)
    event, 		-- :: a -> (b -> a) -> Event b -> a
    fromEvent,		-- :: Event a -> a
    isEvent,		-- :: Event a -> Bool
    isNoEvent,		-- :: Event a -> Bool
    tag, 		-- :: Event a -> b -> Event b,		infixl 8
    tagWith,            -- :: b -> Event a -> Event b,
    attach,		-- :: Event a -> b -> Event (a, b),	infixl 8
    lMerge, 		-- :: Event a -> Event a -> Event a,	infixl 6
    rMerge,		-- :: Event a -> Event a -> Event a,	infixl 6
    merge,		-- :: Event a -> Event a -> Event a,	infixl 6
    mergeBy,		-- :: (a -> a -> a) -> Event a -> Event a -> Event a
    mapMerge,           -- :: (a -> c) -> (b -> c) -> (a -> b -> c) 
                        --    -> Event a -> Event b -> Event c
    mergeEvents,        -- :: [Event a] -> Event a
    catEvents,		-- :: [Event a] -> Event [a]
    joinE,		-- :: Event a -> Event b -> Event (a,b),infixl 7
    splitE,		-- :: Event (a,b) -> (Event a, Event b)
    filterE,	 	-- :: (a -> Bool) -> Event a -> Event a
    mapFilterE,		-- :: (a -> Maybe b) -> Event a -> Event b
    gate,		-- :: Event a -> Bool -> Event a,	infixl 8

) where

import Control.Arrow

import FRP.Yampa.Core
import FRP.Yampa.Diagnostics
import FRP.Yampa.Event
import FRP.Yampa.Miscellany (( # ), dup, swap)
import FRP.Yampa.Integration
import FRP.Yampa.Random
import FRP.Yampa.Simulation
import FRP.Yampa.Switches
import FRP.Yampa.Time
import FRP.Yampa.VectorSpace

sfNever :: SF' a (Event b)
sfNever = sfConst NoEvent

-- The event-processing function *could* accept the present NoEvent
-- output as an extra state argument. That would facilitate composition
-- of event-processing functions somewhat, but would presumably incur an
-- extra cost for the more common and simple case of non-composed event
-- processors.
-- 
sfEP :: (c -> a -> (c, b, b)) -> c -> b -> SF' (Event a) b
sfEP f c bne = sf
    where
        sf = SFEP (\_ ea -> case ea of
                                 NoEvent -> (sf, bne)
                                 Event a -> let
                                                (c', b, bne') = f c a
                                            in
                                                (sfEP f c' bne', b))
                  f
                  c
                  bne


-- epPrim is used to define hold, accum, and other event-processing
-- functions.
epPrim :: (c -> a -> (c, b, b)) -> c -> b -> SF (Event a) b
epPrim f c bne = SF {sfTF = tf0}
    where
        tf0 NoEvent   = (sfEP f c bne, bne)
        tf0 (Event a) = let
                            (c', b, bne') = f c a
                        in
                            (sfEP f c' bne', b)


{-
-- !!! Maybe something like this?
-- !!! But one problem is that the invarying marking would be lost
-- !!! if the signal function is taken apart and re-constructed from
-- !!! the function description and subordinate signal function in
-- !!! cases like SFCpAXA.
sfMkInv :: SF a b -> SF a b
sfMkInv sf = SF {sfTF = ...}

    sfMkInvAux :: SF' a b -> SF' a b
    sfMkInvAux sf@(SFArr _ _) = sf
    -- sfMkInvAux sf@(SFAcc _ _ _ _) = sf
    sfMkInvAux sf@(SFEP _ _ _ _) = sf
    sfMkInvAux sf@(SFCpAXA tf inv fd1 sf2 fd3)
	| inv       = sf
	| otherwise = SFCpAXA tf' True fd1 sf2 fd3
        where
            tf' = \dt a -> let (sf', b) = tf dt a in (sfMkInvAux sf', b)
    sfMkInvAux sf@(SF' tf inv)
        | inv       = sf
        | otherwise = SF' tf' True
            tf' = 

-}

------------------------------------------------------------------------------
-- Basic event sources
------------------------------------------------------------------------------

-- | Event source that never occurs.
never :: SF a (Event b)
never = SF {sfTF = \_ -> (sfNever, NoEvent)}


-- | Event source with a single occurrence at time 0. The value of the event
-- is given by the function argument.
now :: b -> SF a (Event b)
now b0 = (Event b0 --> never)


-- | Event source with a single occurrence at or as soon after (local) time /q/
-- as possible.
after :: Time -- ^ The time /q/ after which the event should be produced
      -> b    -- ^ Value to produce at that time
      -> SF a (Event b)
after q x = afterEach [(q,x)]

-- | Event source with repeated occurrences with interval q.
-- Note: If the interval is too short w.r.t. the sampling intervals,
-- the result will be that events occur at every sample. However, no more
-- than one event results from any sampling interval, thus avoiding an
-- "event backlog" should sampling become more frequent at some later
-- point in time.

-- !!! 2005-03-30:  This is potentially a bit inefficient since we KNOW
-- !!! (at this level) that the SF is going to be invarying. But afterEach
-- !!! does NOT know this as the argument list may well be finite.
-- !!! We could use sfMkInv, but that's not without problems.
-- !!! We're probably better off specializing afterEachCat here.

repeatedly :: Time -> b -> SF a (Event b)
repeatedly q x | q > 0 = afterEach qxs
               | otherwise = usrErr "AFRP" "repeatedly" "Non-positive period."
    where
        qxs = (q,x):qxs        


-- Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- only the first will in fact occur to avoid an event backlog.
-- Question: Should positive periods except for the first one be required?
-- Note that periods of length 0 will always be skipped except for the first.
-- Right now, periods of length 0 is allowed on the grounds that no attempt
-- is made to forbid simultaneous events elsewhere.
{-
afterEach :: [(Time,b)] -> SF a (Event b)
afterEach [] = never
afterEach ((q,x):qxs)
    | q < 0     = usrErr "AFRP" "afterEach" "Negative period."
    | otherwise = SF {sfTF = tf0}
    where
	tf0 _ = if q <= 0 then
                    (scheduleNextEvent 0.0 qxs, Event x)
                else
		    (awaitNextEvent (-q) x qxs, NoEvent)

	scheduleNextEvent t [] = sfNever
        scheduleNextEvent t ((q,x):qxs)
	    | q < 0     = usrErr "AFRP" "afterEach" "Negative period."
	    | t' >= 0   = scheduleNextEvent t' qxs
	    | otherwise = awaitNextEvent t' x qxs
	    where
	        t' = t - q
	awaitNextEvent t x qxs = SF' {sfTF' = tf}
	    where
		tf dt _ | t' >= 0   = (scheduleNextEvent t' qxs, Event x)
		        | otherwise = (awaitNextEvent t' x qxs, NoEvent)
		    where
		        t' = t + dt
-}

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- only the first will in fact occur to avoid an event backlog.

-- After all, after, repeatedly etc. are defined in terms of afterEach.
afterEach :: [(Time,b)] -> SF a (Event b)
afterEach qxs = afterEachCat qxs >>> arr (fmap head)

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- the output list will contain all events produced during that interval.

-- Guaranteed not to miss any events.
afterEachCat :: [(Time,b)] -> SF a (Event [b])
afterEachCat [] = never
afterEachCat ((q,x):qxs)
    | q < 0     = usrErr "AFRP" "afterEachCat" "Negative period."
    | otherwise = SF {sfTF = tf0}
    where
	tf0 _ = if q <= 0 then
                    emitEventsScheduleNext 0.0 [x] qxs
                else
		    (awaitNextEvent (-q) x qxs, NoEvent)

	emitEventsScheduleNext _ xs [] = (sfNever, Event (reverse xs))
        emitEventsScheduleNext t xs ((q,x):qxs)
	    | q < 0     = usrErr "AFRP" "afterEachCat" "Negative period."
	    | t' >= 0   = emitEventsScheduleNext t' (x:xs) qxs
	    | otherwise = (awaitNextEvent t' x qxs, Event (reverse xs))
	    where
	        t' = t - q
	awaitNextEvent t x qxs = SF' tf -- False
	    where
		tf dt _ | t' >= 0   = emitEventsScheduleNext t' [x] qxs
		        | otherwise = (awaitNextEvent t' x qxs, NoEvent)
		    where
		        t' = t + dt

-- | Delay for events. (Consider it a triggered after, hence /basic/.)

-- Can be implemented fairly cheaply as long as the events are sparse.
-- It is a question of rescheduling events for later. Not unlike "afterEach".
--
-- It is not exactly the case that delayEvent t = delay t NoEvent
-- since the rules for dropping/extrapolating samples are different.
-- A single event occurrence will never be duplicated.
-- If there is an event occurrence, one will be output as soon as
-- possible after the given delay time, but not necessarily that
-- one.  See delayEventCat.

delayEvent :: Time -> SF (Event a) (Event a)
delayEvent q | q < 0     = usrErr "AFRP" "delayEvent" "Negative delay."
             | q == 0    = identity
             | otherwise = delayEventCat q >>> arr (fmap head)


-- There is no *guarantee* above that every event actually will be
-- rescheduled since the sampling frequency (temporarily) might drop.
-- The following interface would allow ALL scheduled events to occur
-- as soon as possible:
-- (Read "delay event and catenate events that occur so closely so as to be
-- inseparable".)
-- The events in the list are ordered temporally to the extent possible.

{-
-- This version is too strict!
delayEventCat :: Time -> SF (Event a) (Event [a])
delayEventCat q | q < 0     = usrErr "AFRP" "delayEventCat" "Negative delay."
                | q == 0    = arr (fmap (:[]))
                | otherwise = SF {sfTF = tf0}
    where
	tf0 NoEvent   = (noPendingEvent, NoEvent)
        tf0 (Event x) = (pendingEvents (-q) [] [] (-q) x, NoEvent)

        noPendingEvent = SF' tf -- True
            where
                tf _ NoEvent   = (noPendingEvent, NoEvent)
                tf _ (Event x) = (pendingEvents (-q) [] [] (-q) x, NoEvent)
				 
        -- t_next is the present time w.r.t. the next scheduled event.
        -- t_last is the present time w.r.t. the last scheduled event.
        -- In the event queues, events are associated with their time
	-- w.r.t. to preceding event (positive).
        pendingEvents t_last rqxs qxs t_next x = SF' tf -- True
            where
	        tf dt NoEvent    = tf1 (t_last + dt) rqxs (t_next + dt)
                tf dt (Event x') = tf1 (-q) ((q', x') : rqxs) t_next'
		    where
		        t_next' = t_next  + dt
                        t_last' = t_last  + dt
                        q'      = t_last' + q

                tf1 t_last' rqxs' t_next'
                    | t_next' >= 0 =
                        emitEventsScheduleNext t_last' rqxs' qxs t_next' [x]
		    | otherwise =
                        (pendingEvents t_last' rqxs' qxs t_next' x, NoEvent)

        -- t_next is the present time w.r.t. the *scheduled* time of the
        -- event that is about to be emitted (i.e. >= 0).
        -- The time associated with any event at the head of the event
        -- queue is also given w.r.t. the event that is about to be emitted.
        -- Thus, t_next - q' is the present time w.r.t. the event at the head
        -- of the event queue.
        emitEventsScheduleNext t_last [] [] t_next rxs =
            (noPendingEvent, Event (reverse rxs))
        emitEventsScheduleNext t_last rqxs [] t_next rxs =
            emitEventsScheduleNext t_last [] (reverse rqxs) t_next rxs
        emitEventsScheduleNext t_last rqxs ((q', x') : qxs') t_next rxs
            | q' > t_next = (pendingEvents t_last rqxs qxs' (t_next - q') x',
                             Event (reverse rxs))
            | otherwise   = emitEventsScheduleNext t_last rqxs qxs' (t_next-q')
                                                   (x' : rxs)
-}

-- | Delay an event by a given delta and catenate events that occur so closely
-- so as to be /inseparable/.
delayEventCat :: Time -> SF (Event a) (Event [a])
delayEventCat q | q < 0     = usrErr "AFRP" "delayEventCat" "Negative delay."
                | q == 0    = arr (fmap (:[]))
                | otherwise = SF {sfTF = tf0}
    where
        tf0 e = (case e of
                     NoEvent -> noPendingEvent
                     Event x -> pendingEvents (-q) [] [] (-q) x,
                 NoEvent)

        noPendingEvent = SF' tf -- True
            where
                tf _ e = (case e of
                              NoEvent -> noPendingEvent
                              Event x -> pendingEvents (-q) [] [] (-q) x,
                          NoEvent)
				 
        -- t_next is the present time w.r.t. the next scheduled event.
        -- t_last is the present time w.r.t. the last scheduled event.
        -- In the event queues, events are associated with their time
	-- w.r.t. to preceding event (positive).
        pendingEvents t_last rqxs qxs t_next x = SF' tf -- True
            where
                tf dt e
                    | t_next' >= 0 =
			emitEventsScheduleNext e t_last' rqxs qxs t_next' [x]
                    | otherwise    = 
			(pendingEvents t_last'' rqxs' qxs t_next' x, NoEvent)
                    where
		        t_next' = t_next  + dt
                        t_last' = t_last  + dt 
                        (t_last'', rqxs') =
                            case e of
                                NoEvent  -> (t_last', rqxs)
                                Event x' -> (-q, (t_last'+q,x') : rqxs)

        -- t_next is the present time w.r.t. the *scheduled* time of the
        -- event that is about to be emitted (i.e. >= 0).
        -- The time associated with any event at the head of the event
        -- queue is also given w.r.t. the event that is about to be emitted.
        -- Thus, t_next - q' is the present time w.r.t. the event at the head
        -- of the event queue.
        emitEventsScheduleNext e _ [] [] _ rxs =
            (case e of
                 NoEvent -> noPendingEvent
                 Event x -> pendingEvents (-q) [] [] (-q) x, 
             Event (reverse rxs))
        emitEventsScheduleNext e t_last rqxs [] t_next rxs =
            emitEventsScheduleNext e t_last [] (reverse rqxs) t_next rxs
        emitEventsScheduleNext e t_last rqxs ((q', x') : qxs') t_next rxs
            | q' > t_next = (case e of
                                 NoEvent -> 
				     pendingEvents t_last 
                                                   rqxs 
                                                   qxs'
                                                   (t_next - q')
                                                   x'
                                 Event x'' ->
				     pendingEvents (-q) 
                                                   ((t_last+q, x'') : rqxs)
                                                   qxs'
                                                   (t_next - q')
                                                   x',
                             Event (reverse rxs))
            | otherwise   = emitEventsScheduleNext e
                                                   t_last
                                                   rqxs 
                                                   qxs' 
                                                   (t_next - q')
                                                   (x' : rxs)


-- | A rising edge detector. Useful for things like detecting key presses.
-- It is initialised as /up/, meaning that events occuring at time 0 will
-- not be detected.

-- Note that we initialize the loop with state set to True so that there
-- will not be an occurence at t0 in the logical time frame in which
-- this is started.
edge :: SF Bool (Event ())
edge = iEdge True

-- | A rising edge detector that can be initialized as up ('True', meaning
--   that events occurring at time 0 will not be detected) or down
--   ('False', meaning that events ocurring at time 0 will be detected).
iEdge :: Bool -> SF Bool (Event ())
-- iEdge i = edgeBy (isBoolRaisingEdge ()) i
iEdge b = sscanPrim f (if b then 2 else 0) NoEvent
    where
        f :: Int -> Bool -> Maybe (Int, Event ())
        f 0 False = Nothing
        f 0 True  = Just (1, Event ())
        f 1 False = Just (0, NoEvent)
        f 1 True  = Just (2, NoEvent)
        f 2 False = Just (0, NoEvent)
        f 2 True  = Nothing
        f _ _     = undefined

-- | Like 'edge', but parameterized on the tag value.
edgeTag :: a -> SF Bool (Event a)
-- edgeTag a = edgeBy (isBoolRaisingEdge a) True
edgeTag a = edge >>> arr (`tag` a)


-- Internal utility.
-- isBoolRaisingEdge :: a -> Bool -> Bool -> Maybe a
-- isBoolRaisingEdge _ False False = Nothing
-- isBoolRaisingEdge a False True  = Just a
-- isBoolRaisingEdge _ True  True  = Nothing
-- isBoolRaisingEdge _ True  False = Nothing


-- | Edge detector particularized for detecting transtitions
--   on a 'Maybe' signal from 'Nothing' to 'Just'.

-- !!! 2005-07-09: To be done or eliminated
-- !!! Maybe could be kept as is, but could be easy to implement directly
-- !!! in terms of sscan?
edgeJust :: SF (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
    where
        isJustEdge Nothing  Nothing     = Nothing
        isJustEdge Nothing  ma@(Just _) = ma
        isJustEdge (Just _) (Just _)    = Nothing
        isJustEdge (Just _) Nothing     = Nothing


-- | Edge detector parameterized on the edge detection function and initial
-- state, i.e., the previous input sample. The first argument to the
-- edge detection function is the previous sample, the second the current one.

-- !!! Is this broken!?! Does not disallow an edge condition that persists
-- !!! between consecutive samples. See discussion in ToDo list above.
-- !!! 2005-07-09: To be done.
edgeBy :: (a -> a -> Maybe b) -> a -> SF a (Event b)
edgeBy isEdge a_init = SF {sfTF = tf0}
    where
	tf0 a0 = (ebAux a0, maybeToEvent (isEdge a_init a0))

	ebAux a_prev = SF' tf -- True
	    where
		tf _ a = (ebAux a, maybeToEvent (isEdge a_prev a))


------------------------------------------------------------------------------
-- Stateful event suppression
------------------------------------------------------------------------------

-- | Suppression of initial (at local time 0) event.
notYet :: SF (Event a) (Event a)
notYet = initially NoEvent


-- | Suppress all but the first event.
once :: SF (Event a) (Event a)
once = takeEvents 1


-- | Suppress all but the first n events.
takeEvents :: Int -> SF (Event a) (Event a)
takeEvents n | n <= 0 = never
takeEvents n = dSwitch (arr dup) (const (NoEvent >-- takeEvents (n - 1)))


{-
-- More complicated using "switch" that "dSwitch".
takeEvents :: Int -> SF (Event a) (Event a)
takeEvents 0       = never
takeEvents (n + 1) = switch (never &&& identity) (takeEvents' n)
    where
        takeEvents' 0       a = now a
        takeEvents' (n + 1) a = switch (now a &&& notYet) (takeEvents' n)
-}


-- | Suppress first n events.

-- Here dSwitch or switch does not really matter.
dropEvents :: Int -> SF (Event a) (Event a)
dropEvents n | n <= 0  = identity
dropEvents n = dSwitch (never &&& identity)
                             (const (NoEvent >-- dropEvents (n - 1)))

------------------------------------------------------------------------------
-- Wave-form generation
------------------------------------------------------------------------------

-- | Zero-order hold.

-- !!! Should be redone using SFSScan?
-- !!! Otherwise, we are missing an invarying case.
old_hold :: a -> SF (Event a) a
old_hold a_init = switch (constant a_init &&& identity)
                         ((NoEvent >--) . old_hold)

-- | Zero-order hold.
hold :: a -> SF (Event a) a
hold a_init = epPrim f () a_init
    where
        f _ a = ((), a, a)

-- !!!
-- !!! 2005-04-10: I DO NO LONGER THINK THIS IS CORRECT!
-- !!! CAN ONE POSSIBLY GET THE DESIRED STRICTNESS PROPERTIES
-- !!! ("DECOUPLING") this way???
-- !!! Also applies to the other "d" functions that were tentatively
-- !!! defined using only epPrim.
-- !!!
-- !!! 2005-06-13: Yes, indeed wrong! (But it's subtle, one has to
-- !!! make sure that the incoming event (and not just the payload
-- !!! of the event) is control dependent on  the output of "dHold"
-- !!! to observe it.
-- !!!
-- !!! 2005-06-09: But if iPre can be defined in terms of sscan,
-- !!! and ep + sscan = sscan, then things might work, and
-- !!! it might be possible to define dHold simply as hold >>> iPre
-- !!! without any performance penalty. 

-- | Zero-order hold with delay.
--
-- Identity: dHold a0 = hold a0 >>> iPre a0).
dHold :: a -> SF (Event a) a
dHold a0 = hold a0 >>> iPre a0
{-
-- THIS IS WRONG! SEE ABOVE.
dHold a_init = epPrim f a_init a_init
    where
        f a' a = (a, a', a)
-}

-- | Tracks input signal when available, holds last value when disappears.
--
-- !!! DANGER!!! Event used inside arr! Probably OK because arr will not be
-- !!! optimized to arrE. But still. Maybe rewrite this using, say, scan?
-- !!! or switch? Switching (in hold) for every input sample does not
-- !!! seem like such a great idea anyway.
trackAndHold :: a -> SF (Maybe a) a
trackAndHold a_init = arr (maybe NoEvent Event) >>> hold a_init


------------------------------------------------------------------------------
-- Accumulators
------------------------------------------------------------------------------

-- | See 'accum'.
old_accum :: a -> SF (Event (a -> a)) (Event a)
old_accum = accumBy (flip ($))

-- | Given an initial value in an accumulator,
--   it returns a signal function that processes
--   an event carrying transformation functions.
--   Every time an 'Event' is received, the function
--   inside it is applied to the accumulator,
--   whose new value is outputted in an 'Event'.
--   
accum :: a -> SF (Event (a -> a)) (Event a)
accum a_init = epPrim f a_init NoEvent
    where
        f a g = (a', Event a', NoEvent) -- Accumulator, output if Event, output if no event
            where
                a' = g a


-- | Zero-order hold accumulator (always produces the last outputted value
--   until an event arrives).
accumHold :: a -> SF (Event (a -> a)) a
accumHold a_init = epPrim f a_init a_init
    where
        f a g = (a', a', a') -- Accumulator, output if Event, output if no event
            where
                a' = g a

-- | Zero-order hold accumulator with delayed initialization (always produces
-- the last outputted value until an event arrives, but the very initial output 
-- is always the given accumulator).
dAccumHold :: a -> SF (Event (a -> a)) a
dAccumHold a_init = accumHold a_init >>> iPre a_init
{-
-- WRONG!
-- epPrim DOES and MUST patternmatch
-- on the input at every time step.
-- Test case to check for this added!
dAccumHold a_init = epPrim f a_init a_init
    where
        f a g = (a', a, a')
            where
                a' = g a
-}


-- | See 'accumBy'.
old_accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
old_accumBy f b_init = switch (never &&& identity) $ \a -> abAux (f b_init a)
    where
        abAux b = switch (now b &&& notYet) $ \a -> abAux (f b a)

-- | Accumulator parameterized by the accumulation function.
accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
accumBy g b_init = epPrim f b_init NoEvent
    where
        f b a = (b', Event b', NoEvent)
            where
                b' = g b a

-- | Zero-order hold accumulator parameterized by the accumulation function.
accumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
accumHoldBy g b_init = epPrim f b_init b_init
    where
        f b a = (b', b', b')
            where
                b' = g b a

-- !!! This cannot be right since epPrim DOES and MUST patternmatch
-- !!! on the input at every time step.
-- !!! Add a test case to check for this!

-- | Zero-order hold accumulator parameterized by the accumulation function
--   with delayed initialization (initial output sample is always the
--   given accumulator).
dAccumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
dAccumHoldBy f a_init = accumHoldBy f a_init >>> iPre a_init
{-
-- WRONG!
-- epPrim DOES and MUST patternmatch
-- on the input at every time step.
-- Test case to check for this added!
dAccumHoldBy g b_init = epPrim f b_init b_init
    where
        f b a = (b', b, b')
            where
                b' = g b a
-}


{- Untested:

accumBy f b = switch (never &&& identity) $ \a ->
              let b' = f b a in NoEvent >-- Event b' --> accumBy f b'

But no real improvement in clarity anyway.

-}

-- accumBy f b = accumFilter (\b -> a -> let b' = f b a in (b', Event b')) b

{-
-- Identity: accumBy f = accumFilter (\b a -> let b' = f b a in (b',Just b'))
accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
accumBy f b_init = SF {sfTF = tf0}
    where
        tf0 NoEvent    = (abAux b_init, NoEvent) 
        tf0 (Event a0) = let b' = f b_init a0
		         in (abAux b', Event b')

        abAux b = SF' {sfTF' = tf}
	    where
		tf _ NoEvent   = (abAux b, NoEvent)
		tf _ (Event a) = let b' = f b a
			         in (abAux b', Event b')
-}

{-
accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
accumFilter f c_init = SF {sfTF = tf0}
    where
        tf0 NoEvent    = (afAux c_init, NoEvent) 
        tf0 (Event a0) = case f c_init a0 of
		             (c', Nothing) -> (afAux c', NoEvent)
			     (c', Just b0) -> (afAux c', Event b0)

        afAux c = SF' {sfTF' = tf}
	    where
		tf _ NoEvent   = (afAux c, NoEvent)
		tf _ (Event a) = case f c a of
			             (c', Nothing) -> (afAux c', NoEvent)
				     (c', Just b)  -> (afAux c', Event b)
-}

-- | See 'accumFilter'.
old_accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
old_accumFilter f c_init = switch (never &&& identity) $ \a -> afAux (f c_init a)
    where
        afAux (c, Nothing) = switch (never &&& notYet) $ \a -> afAux (f c a)
        afAux (c, Just b)  = switch (now b &&& notYet) $ \a -> afAux (f c a)

-- | Accumulator parameterized by the accumulator function with filtering,
--   possibly discarding some of the input events based on whether the second
--   component of the result of applying the accumulation function is
--   'Nothing' or 'Just' x for some x.
accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
accumFilter g c_init = epPrim f c_init NoEvent
    where
        f c a = case g c a of
                    (c', Nothing) -> (c', NoEvent, NoEvent)
                    (c', Just b)  -> (c', Event b, NoEvent)


------------------------------------------------------------------------------
-- Delays
------------------------------------------------------------------------------

-- | Uninitialized delay operator (old implementation).

-- !!! The seq helps in the dynamic delay line example. But is it a good
-- !!! idea in general? Are there other accumulators which should be seq'ed
-- !!! as well? E.g. accum? Switch? Anywhere else? What's the underlying
-- !!! design principle? What can the user assume?
--
old_pre :: SF a a
old_pre = SF {sfTF = tf0}
    where
        tf0 a0 = (preAux a0, usrErr "AFRP" "pre" "Uninitialized pre operator.")

	preAux a_prev = SF' tf -- True
	    where
		tf _ a = {- a_prev `seq` -} (preAux a, a_prev)

-- | Initialized delay operator (old implementation).
old_iPre :: a -> SF a a
old_iPre = (--> old_pre)



-- | Uninitialized delay operator.

-- !!! Redefined using SFSScan
-- !!! About 20% slower than old_pre on its own.
pre :: SF a a
pre = sscanPrim f uninit uninit
    where
        f c a = Just (a, c)
        uninit = usrErr "AFRP" "pre" "Uninitialized pre operator."


-- | Initialized delay operator.
iPre :: a -> SF a a
iPre = (--> pre)


------------------------------------------------------------------------------
-- Timed delays
------------------------------------------------------------------------------

-- | Delay a signal by a fixed time 't', using the second parameter
-- to fill in the initial 't' seconds.

-- Invariants:
-- t_diff measure the time since the latest output sample ideally
-- should have been output. Whenever that equals or exceeds the
-- time delta for the next buffered sample, it is time to output a
-- new sample (although not necessarily the one first in the queue:
-- it might be necessary to "catch up" by discarding samples.
-- 0 <= t_diff < bdt, where bdt is the buffered time delta for the
-- sample on the front of the buffer queue.
--
-- Sum of time deltas in the queue >= q.

-- !!! PROBLEM!
-- Since input samples sometimes need to be duplicated, it is not a
-- good idea use a delay on things like events since we then could
-- end up with duplication of event occurrences.
-- (Thus, we actually NEED delayEvent.)

delay :: Time -> a -> SF a a
delay q a_init | q < 0     = usrErr "AFRP" "delay" "Negative delay."
               | q == 0    = identity
               | otherwise = SF {sfTF = tf0}
    where
        tf0 a0 = (delayAux [] [(q, a0)] 0 a_init, a_init)

        delayAux _ [] _ _ = undefined
        delayAux rbuf buf@((bdt, ba) : buf') t_diff a_prev = SF' tf -- True
            where
                tf dt a | t_diff' < bdt =
                              (delayAux rbuf' buf t_diff' a_prev, a_prev)
                        | otherwise = nextSmpl rbuf' buf' (t_diff' - bdt) ba
                    where
        	        t_diff' = t_diff + dt
        	        rbuf'   = (dt, a) : rbuf
    
                        nextSmpl rbuf [] t_diff a =
                            nextSmpl [] (reverse rbuf) t_diff a
                        nextSmpl rbuf buf@((bdt, ba) : buf') t_diff a
                            | t_diff < bdt = (delayAux rbuf buf t_diff a, a)
                            | otherwise    = nextSmpl rbuf buf' (t_diff-bdt) ba
                

-- !!! Hmm. Not so easy to do efficiently, it seems ...

-- varDelay :: Time -> a -> SF (a, Time) a
-- varDelay = undefined


------------------------------------------------------------------------------
-- Variable pause in signal
------------------------------------------------------------------------------

-- | Given a value in an accumulator (b), a predicate signal function (sfC), 
--   and a second signal function (sf), pause will produce the accumulator b
--   if sfC input is True, and will transform the signal using sf otherwise.
--   It acts as a pause with an accumulator for the moments when the
--   transformation is paused.
pause :: b -> SF a Bool -> SF a b -> SF a b
pause b_init (SF { sfTF = tfP}) (SF {sfTF = tf10}) = SF {sfTF = tf0}
 where
       -- Initial transformation (no time delta):
       -- If the condition is True, return the accumulator b_init)
       -- Otherwise transform the input normally and recurse.
       tf0 a0 = case tfP a0 of
                 (c, True)  -> (pauseInit b_init tf10 c, b_init)
                 (c, False) -> let (k, b0) = tf10 a0
                               in (pause' b0 k c, b0)

       -- Similar deal, but with a time delta
       pauseInit :: b -> (a -> Transition a b) -> SF' a Bool -> SF' a b
       pauseInit b_init' tf10' c = SF' tf0'
         where tf0' dt a =
                case (sfTF' c) dt a of
                  (c', True)  -> (pauseInit b_init' tf10' c', b_init')
                  (c', False) -> let (k, b0) = tf10' a
                                 in (pause' b0 k c', b0)

       -- Very same deal (almost alpha-renameable)
       pause' :: b -> SF' a b -> SF' a Bool -> SF' a b
       pause' b_init' tf10' tfP' = SF' tf0'
         where tf0' dt a = 
                 case (sfTF' tfP') dt a of
                   (tfP'', True) -> (pause' b_init' tf10' tfP'', b_init')
                   (tfP'', False) -> let (tf10'', b0') = (sfTF' tf10') dt a
                                     in (pause' b0' tf10'' tfP'', b0')

-- if_then_else :: SF a Bool -> SF a b -> SF a b -> SF a b
-- if_then_else condSF sfThen sfElse = proc (i) -> do
--   cond  <- condSF -< i
--   ok    <- sfThen -< i
--   notOk <- sfElse -< i
--   returnA -< if cond then ok else notOk

------------------------------------------------------------------------------
-- Loops with guaranteed well-defined feedback
------------------------------------------------------------------------------

-- | Loop with an initial value for the signal being fed back.
loopPre :: c -> SF (a,c) (b,c) -> SF a b
loopPre c_init sf = loop (second (iPre c_init) >>> sf)

-- | Loop by integrating the second value in the pair and feeding the
-- result back. Because the integral at time 0 is zero, this is always
-- well defined.
loopIntegral :: VectorSpace c s => SF (a,c) (b,c) -> SF a b
loopIntegral sf = loop (second integral >>> sf)

-- Vim modeline
-- vim:set tabstop=8 expandtab:
