-- |
-- Module      :  FRP.Yampa.EventS
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Event Signal Functions and SF combinators.
--
-- Events represent values that only exist instantaneously, at discrete points
-- in time. Examples include mouse clicks, zero-crosses of monotonic continuous
-- signals, and square waves.
--
-- For signals that carry events, there should be a limit in the number of
-- events we can observe in a time period, no matter how much we increase the
-- sampling frequency.
module FRP.Yampa.EventS
    (
      -- * Basic event sources
      never
    , now
    , after
    , repeatedly
    , afterEach
    , afterEachCat
    , delayEvent
    , delayEventCat
    , edge
    , iEdge
    , edgeTag
    , edgeJust
    , edgeBy

      -- * Stateful event suppression
    , notYet
    , once
    , takeEvents
    , dropEvents

      -- * Hybrid SF combinators
    , snap
    , snapAfter
    , sample
    , sampleWindow

      -- * Repetition and switching
    , recur
    , andThen
    )
  where

import Control.Arrow

import FRP.Yampa.Arrow
import FRP.Yampa.Basic
import FRP.Yampa.Diagnostics
import FRP.Yampa.Event
import FRP.Yampa.Hybrid
import FRP.Yampa.InternalCore (SF (..), SF' (..), Time, sfConst)
import FRP.Yampa.Scan
import FRP.Yampa.Switches

infixr 5 `andThen`

-- * Basic event sources

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
repeatedly :: Time -> b -> SF a (Event b)
repeatedly q x | q > 0 = afterEach qxs
               | otherwise = usrErr "Yampa" "repeatedly" "Non-positive period."
  where
    qxs = (q,x):qxs

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- only the first will in fact occur to avoid an event backlog.
afterEach :: [(Time,b)] -> SF a (Event b)
afterEach qxs = afterEachCat qxs >>> arr (fmap head)

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- the output list will contain all events produced during that interval.
afterEachCat :: [(Time,b)] -> SF a (Event [b])
afterEachCat [] = never
afterEachCat ((q,x):qxs)
    | q < 0     = usrErr "Yampa" "afterEachCat" "Negative period."
    | otherwise = SF {sfTF = tf0}
  where
    tf0 _ = if q <= 0
              then emitEventsScheduleNext 0.0 [x] qxs
              else (awaitNextEvent (-q) x qxs, NoEvent)

    emitEventsScheduleNext _ xs [] = (sfNever, Event (reverse xs))
    emitEventsScheduleNext t xs ((q,x):qxs)
        | q < 0     = usrErr "Yampa" "afterEachCat" "Negative period."
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
delayEvent :: Time -> SF (Event a) (Event a)
delayEvent q | q < 0     = usrErr "Yampa" "delayEvent" "Negative delay."
             | q == 0    = identity
             | otherwise = delayEventCat q >>> arr (fmap head)

-- | Delay an event by a given delta and catenate events that occur so closely
-- so as to be /inseparable/.
delayEventCat :: Time -> SF (Event a) (Event [a])
delayEventCat q | q < 0     = usrErr "Yampa" "delayEventCat" "Negative delay."
                | q == 0    = arr (fmap (:[]))
                | otherwise = SF {sfTF = tf0}
  where
    tf0 e = ( case e of
                NoEvent -> noPendingEvent
                Event x -> pendingEvents (-q) [] [] (-q) x
            , NoEvent
            )

    noPendingEvent = SF' tf -- True
      where
        tf _ e = ( case e of
                     NoEvent -> noPendingEvent
                     Event x -> pendingEvents (-q) [] [] (-q) x
                 , NoEvent
                 )

    -- t_next is the present time w.r.t. the next scheduled event.
    -- t_last is the present time w.r.t. the last scheduled event.
    -- In the event queues, events are associated with their time
    -- w.r.t. to preceding event (positive).
    pendingEvents t_last rqxs qxs t_next x = SF' tf -- True
      where
        tf dt e
            | t_next' >= 0
            = emitEventsScheduleNext e t_last' rqxs qxs t_next' [x]
            | otherwise
            = (pendingEvents t_last'' rqxs' qxs t_next' x, NoEvent)
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
      ( case e of
          NoEvent -> noPendingEvent
          Event x -> pendingEvents (-q) [] [] (-q) x
      , Event (reverse rxs)
      )
    emitEventsScheduleNext e t_last rqxs [] t_next rxs =
      emitEventsScheduleNext e t_last [] (reverse rqxs) t_next rxs
    emitEventsScheduleNext e t_last rqxs ((q', x') : qxs') t_next rxs
      | q' > t_next = ( case e of
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
                                          x'
                      , Event (reverse rxs)
                      )
      | otherwise   = emitEventsScheduleNext e
                                             t_last
                                             rqxs
                                             qxs'
                                             (t_next - q')
                                             (x' : rxs)

-- | A rising edge detector. Useful for things like detecting key presses.
-- It is initialised as /up/, meaning that events occurring at time 0 will
-- not be detected.
edge :: SF Bool (Event ())
edge = iEdge True

-- | A rising edge detector that can be initialized as up ('True', meaning
--   that events occurring at time 0 will not be detected) or down
--   ('False', meaning that events occurring at time 0 will be detected).
iEdge :: Bool -> SF Bool (Event ())
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
edgeTag a = edge >>> arr (`tag` a)

-- | Edge detector particularized for detecting transitions
--   on a 'Maybe' signal from 'Nothing' to 'Just'.
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
edgeBy :: (a -> a -> Maybe b) -> a -> SF a (Event b)
edgeBy isEdge a_init = SF {sfTF = tf0}
  where
    tf0 a0 = (ebAux a0, maybeToEvent (isEdge a_init a0))

    ebAux a_prev = SF' tf -- True
      where
        tf _ a = (ebAux a, maybeToEvent (isEdge a_prev a))

-- * Stateful event suppression

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

-- | Suppress first n events.
dropEvents :: Int -> SF (Event a) (Event a)
dropEvents n | n <= 0  = identity
dropEvents n =
  -- Here dSwitch or switch does not really matter.
  dSwitch (never &&& identity)
          (const (NoEvent >-- dropEvents (n - 1)))

-- ** Hybrid continuous-to-discrete SF combinators.

-- | Event source with a single occurrence at time 0. The value of the event is
-- obtained by sampling the input at that time.
snap :: SF a (Event a)
snap =
  -- switch ensures that the entire signal function will become just
  -- "constant" once the sample has been taken.
  switch (never &&& (identity &&& now () >>^ \(a, e) -> e `tag` a)) now

-- | Event source with a single occurrence at or as soon after (local) time
-- @t_ev@ as possible. The value of the event is obtained by sampling the input
-- a that time.
snapAfter :: Time -> SF a (Event a)
snapAfter t_ev =
  switch (never &&& (identity &&& after t_ev () >>^ \(a, e) -> e `tag` a)) now

-- | Sample a signal at regular intervals.
sample :: Time -> SF a (Event a)
sample p_ev = identity &&& repeatedly p_ev () >>^ \(a, e) -> e `tag` a

-- | Window sampling
--
-- First argument is the window length wl, second is the sampling interval t.
-- The output list should contain (min (truncate (T/t) wl)) samples, where
-- T is the time the signal function has been running. This requires some
-- care in case of sparse sampling. In case of sparse sampling, the
-- current input value is assumed to have been present at all points where
-- sampling was missed.
sampleWindow :: Int -> Time -> SF a (Event [a])
sampleWindow wl q =
    identity &&& afterEachCat (repeat (q, ()))
    >>> arr (\(a, e) -> fmap (map (const a)) e)
    >>> accumBy updateWindow []
  where
    updateWindow w as = drop (max (length w' - wl) 0) w'
      where w' = w ++ as

-- * Repetition and switching

-- | Makes an event source recurring by restarting it as soon as it has an
-- occurrence.
recur :: SF a (Event b) -> SF a (Event b)
recur sfe = switch (never &&& sfe) $ \b -> Event b --> (recur (NoEvent-->sfe))

-- | Apply the first SF until it produces an event, and, afterwards, switch to
-- the second SF. This is just a convenience function, used to write what
-- sometimes is more understandable switch-based code.
andThen :: SF a (Event b) -> SF a (Event b) -> SF a (Event b)
sfe1 `andThen` sfe2 = dSwitch (sfe1 >>^ dup) (const sfe2)
