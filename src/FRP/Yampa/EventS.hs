{-# LANGUAGE GADTs, Rank2Types, CPP      #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.EventS
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.EventS (

    -- * Basic event sources
    never,              -- :: SF a (Event b)
    now,                -- :: b -> SF a (Event b)
    after,              -- :: Time -> b -> SF a (Event b)
    repeatedly,         -- :: Time -> b -> SF a (Event b)
    afterEach,          -- :: [(Time,b)] -> SF a (Event b)
    afterEachCat,       -- :: [(Time,b)] -> SF a (Event [b])
    delayEvent,         -- :: Time -> SF (Event a) (Event a)
    delayEventCat,      -- :: Time -> SF (Event a) (Event [a])
    edge,               -- :: SF Bool (Event ())
    iEdge,              -- :: Bool -> SF Bool (Event ())
    edgeTag,            -- :: a -> SF Bool (Event a)
    edgeJust,           -- :: SF (Maybe a) (Event a)
    edgeBy,             -- :: (a -> a -> Maybe b) -> a -> SF a (Event b)

    -- * Stateful event suppression
    notYet,             -- :: SF (Event a) (Event a)
    once,               -- :: SF (Event a) (Event a)
    takeEvents,         -- :: Int -> SF (Event a) (Event a)
    dropEvents,         -- :: Int -> SF (Event a) (Event a)

    -- ** Pointwise functions on events
    -- noEvent,            -- :: Event a
    -- noEventFst,         -- :: (Event a, b) -> (Event c, b)
    -- noEventSnd,         -- :: (a, Event b) -> (a, Event c)
    -- event,              -- :: a -> (b -> a) -> Event b -> a
    -- fromEvent,          -- :: Event a -> a
    -- isEvent,            -- :: Event a -> Bool
    -- isNoEvent,          -- :: Event a -> Bool
    -- tag,                -- :: Event a -> b -> Event b,          infixl 8
    -- tagWith,            -- :: b -> Event a -> Event b,
    -- attach,             -- :: Event a -> b -> Event (a, b),     infixl 8
    -- lMerge,             -- :: Event a -> Event a -> Event a,    infixl 6
    -- rMerge,             -- :: Event a -> Event a -> Event a,    infixl 6
    -- merge,              -- :: Event a -> Event a -> Event a,    infixl 6
    -- mergeBy,            -- :: (a -> a -> a) -> Event a -> Event a -> Event a
    -- mapMerge,           -- :: (a -> c) -> (b -> c) -> (a -> b -> c)
    --                     --    -> Event a -> Event b -> Event c
    -- mergeEvents,        -- :: [Event a] -> Event a
    -- catEvents,          -- :: [Event a] -> Event [a]
    -- joinE,              -- :: Event a -> Event b -> Event (a,b),infixl 7
    -- splitE,             -- :: Event (a,b) -> (Event a, Event b)
    -- filterE,            -- :: (a -> Bool) -> Event a -> Event a
    -- mapFilterE,         -- :: (a -> Maybe b) -> Event a -> Event b
    -- gate,               -- :: Event a -> Bool -> Event a,       infixl 8
    -- Event sources
    snap,         -- :: SF a (Event a)
    snapAfter,    -- :: Time -> SF a (Event a)
    sample,       -- :: Time -> SF a (Event a)
    recur,        -- :: SF a (Event b) -> SF a (Event b)
    andThen       -- :: SF a (Event b)->SF a (Event b)->SF a (Event b)



) where

import Control.Arrow

import FRP.Yampa.InternalCore (SF(..), sfConst, Time, SF'(..))

import FRP.Yampa.Basic
import FRP.Yampa.Diagnostics
import FRP.Yampa.Event
import FRP.Yampa.Miscellany
import FRP.Yampa.Scan
import FRP.Yampa.Switches


infixr 5 `andThen`

-- -- The event-processing function *could* accept the present NoEvent
-- -- output as an extra state argument. That would facilitate composition
-- -- of event-processing functions somewhat, but would presumably incur an
-- -- extra cost for the more common and simple case of non-composed event
-- -- processors.
--
-- sfEP :: (c -> a -> (c, b, b)) -> c -> b -> SF' (Event a) b
-- sfEP f c bne = sf
--     where
--         sf = SFEP (\_ ea -> case ea of
--                                  NoEvent -> (sf, bne)
--                                  Event a -> let
--                                                 (c', b, bne') = f c a
--                                             in
--                                                 (sfEP f c' bne', b))
--                   f
--                   c
--                   bne
--
--
-- -- epPrim is used to define hold, accum, and other event-processing
-- -- functions.
-- epPrim :: (c -> a -> (c, b, b)) -> c -> b -> SF (Event a) b
-- epPrim f c bne = SF {sfTF = tf0}
--     where
--         tf0 NoEvent   = (sfEP f c bne, bne)
--         tf0 (Event a) = let
--                             (c', b, bne') = f c a
--                         in
--                             (sfEP f c' bne', b)


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
{-# ANN never "HLint: ignore Use const" #-}
never :: SF a (Event b)
never = SF {sfTF = \_ -> (sfNever, NoEvent)}

sfNever :: SF' a (Event b)
sfNever = sfConst NoEvent

-- | Event source with a single occurrence at time 0. The value of the event
-- is given by the function argument.
now :: b -> SF a (Event b)
now b0 = Event b0 --> never


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

-- Event source with a single occurrence at time 0. The value of the event
-- is obtained by sampling the input at that time.
-- (The outer "switch" ensures that the entire signal function will become
-- just "constant" once the sample has been taken.)
snap :: SF a (Event a)
snap = switch (never &&& (identity &&& now () >>^ \(a, e) -> e `tag` a)) now


-- Event source with a single occurrence at or as soon after (local) time t_ev
-- as possible. The value of the event is obtained by sampling the input a
-- that time.
snapAfter :: Time -> SF a (Event a)
snapAfter t_ev = switch (never
             &&& (identity
                  &&& after t_ev () >>^ \(a, e) -> e `tag` a))
            now


-- Sample a signal at regular intervals.
sample :: Time -> SF a (Event a)
sample p_ev = identity &&& repeatedly p_ev () >>^ \(a, e) -> e `tag` a


-- Makes an event source recurring by restarting it as soon as it has an
-- occurrence.
-- !!! What about event sources that have an instantaneous occurrence?
-- !!! E.g. recur (now ()).
-- !!! Or worse, what about recur identity? (or substitute identity for
-- !!! a more sensible definition that e.g. merges any incoming event
-- !!! with an internally generated one, for example)
-- !!! Possibly we should ignore instantaneous reoccurrences.
-- New definition:
recur :: SF a (Event b) -> SF a (Event b)
recur sfe = switch (never &&& sfe) $ \b -> Event b --> (recur (NoEvent-->sfe))

andThen :: SF a (Event b) -> SF a (Event b) -> SF a (Event b)
sfe1 `andThen` sfe2 = dSwitch (sfe1 >>^ dup) (const sfe2)

{-
recur :: SF a (Event b) -> SF a (Event b)
recur sfe = switch (never &&& sfe) recurAux
    where
    recurAux b = switch (now b &&& sfe) recurAux
-}

-- Vim modeline
-- vim:set tabstop=8 expandtab:
