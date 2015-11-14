-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Event
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- Definition of Yampa Event type.
--
-- Note on naming conventions used in this module.
--
-- Names here might have to be rethought. It's really a bit messy.
-- In general, the aim has been short and convenient names (like 'tag',
-- 'attach', 'lMerge') and thus we have tried to stay away from suffixing/
-- prefixing conventions. E.g. 'Event' as a common suffix would be very
-- verbose.
--
-- However, part of the names come from a desire to stay close to similar
-- functions for the Maybe type. e.g. 'event', 'fromEvent', 'isEvent'.
-- In many cases, this use of 'Event' can could understood to refer to the
-- constructor 'Event', not to the type name 'Event'. Thus this use of
-- event should not be seen as a suffixing-with-type-name convention. But
-- that is obviously not easy to see, and, more over, interpreting 'Event'
-- as the name of the type might make equally good or better sense. E.g.
-- 'fromEvent' can also be seen as a function taking an event signal,
-- which is a partial function on time, to a normal signal. The latter is
-- then undefined when the source event function is undefined.
--
-- In other cases, it has been necessary to somehow stay out of the way of
-- names used by the prelude or other commonly imported modules/modules
-- which could be expected to be used heavily in Yampa code. In those cases
-- a suffix 'E' have been added. Examples are 'filterE' (exists in Prelude)
-- and 'joinE' (exists in Monad). Maybe the suffix isn't necessary in the
-- last case.
--
-- Some functions (actually only one currently, 'mapFilterE') have got an 'E'
-- suffix just because they're closely related (by name or semantics) to one
-- which already has an 'E' suffix. Another candidate would be 'splitE' to
-- complement 'joinE'. But events carrying pairs could obviously have other
-- sources than a 'joinE', so currently it is called 'split'.
--
-- 2003-05-19: Actually, have now changed to 'splitE' to avoid a clash
-- with the method 'split' in the class RandomGen.
--
-- 2003-05-19: What about 'gate'? Stands out compared to e.g. 'filterE'.
--
-- Currently the 'E' suffix is considered an exception. Maybe we should use
-- completely different names to avoid the 'E' suffix. If the functions
-- are not used that often, 'Event' might be approriate. Alternatively the
-- suffix 'E' should be adopted globaly (except if the name already contains
-- 'event' in some form?).
--
-- Arguably, having both a type 'Event' and a constructor 'Event' is confusing
-- since there are more than one constructor. But the name 'Event' for the
-- constructor is quite apt. It's really the type name that is wrong. But
-- no one has found a better name, and changing it would be a really major
-- undertaking. Yes, the constructor 'Event' is not exported, but we still
-- need to talk conceptually about them. On the other hand, if we consider
-- Event-signals as partial functions on time, maybe it isn't so confusing:
-- they just don't have a value between events, so 'NoEvent' does not really
-- exist conceptually.
--
-- ToDo:
-- - Either: reveal NoEvent and Event
--   or:     introcuce 'event = Event', call what's now 'event' 'fromEvent',
--           and call what's now called 'fromEvent' something else, like
--           'unsafeFromEvent'??? Better, dump it! After all, using current
--           names, 'fromEvent = event undefined'!
-----------------------------------------------------------------------------------------

module FRP.Yampa.Event where

-- Event is an instance of Functor, Eq, and Ord. Some method instances:
-- fmap :: (a -> b) -> Event a -> Event b
-- (==)     :: Event a -> Event a -> Bool
-- (<=) :: Event a -> Event a -> Bool

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Data.Functor

import FRP.Yampa.Diagnostics
import FRP.Yampa.Forceable


infixl 8 `tag`, `attach`, `gate`
infixl 7 `joinE`
infixl 6 `lMerge`, `rMerge`, `merge`


------------------------------------------------------------------------------
-- The Event type
------------------------------------------------------------------------------

-- The type Event represents a single possible event occurrence.
-- It is isomorphic to Maybe, but its constructors are not exposed outside
-- the AFRP implementation.
-- There could possibly be further constructors, but note that the NeverEvent-
-- idea does not work, at least not in the current AFRP implementation.
-- Also note that it unfortunately is possible to partially break the
-- abstractions through judicious use of e.g. snap and switching.

-- | A single possible event occurrence, that is, a value that may or may
-- not occur. Events are used to represent values that are not produced
-- continuously, such as mouse clicks (only produced when the mouse is clicked,
-- as opposed to mouse positions, which are always defined).
data Event a = NoEvent | Event a deriving (Show)

-- | Make the NoEvent constructor available. Useful e.g. for initialization,
-- ((-->) & friends), and it's easily available anyway (e.g. mergeEvents []).
noEvent :: Event a
noEvent = NoEvent


-- | Suppress any event in the first component of a pair.
noEventFst :: (Event a, b) -> (Event c, b)
noEventFst (_, b) = (NoEvent, b)


-- | Suppress any event in the second component of a pair.
noEventSnd :: (a, Event b) -> (a, Event c)
noEventSnd (a, _) = (a, NoEvent)


-- | Eq instance (equivalent to derived instance)
instance Eq a => Eq (Event a) where
    -- | Equal if both NoEvent or both Event carrying equal values.
    NoEvent   == NoEvent   = True
    (Event x) == (Event y) = x == y
    _         == _         = False


-- | Ord instance (equivalent to derived instance)
instance Ord a => Ord (Event a) where
    -- | NoEvent is smaller than Event, Event x < Event y if x < y
    compare NoEvent   NoEvent   = EQ
    compare NoEvent   (Event _) = LT
    compare (Event _) NoEvent   = GT
    compare (Event x) (Event y) = compare x y

-- | Functor instance (could be derived).
instance Functor Event where
    -- | Apply function to value carried by 'Event', if any.
    fmap _ NoEvent   = NoEvent
    fmap f (Event a) = Event (f a)


-- | Applicative instance (similar to 'Maybe').
instance Applicative Event where
    -- | Wrap a pure value in an 'Event'.
    pure = Event
    -- | If any value (function or arg) is 'NoEvent', everything is.
    NoEvent <*> _ = NoEvent
    Event f <*> x = f <$> x

-- | Monad instance
instance Monad Event where
    -- | Combine events, return 'NoEvent' if any value in the
    -- sequence is 'NoEvent'.
    (Event x) >>= k = k x
    NoEvent  >>= _  = NoEvent

    (>>) = (*>)

    -- | See 'pure'.
    return          = pure
    -- | Fail with 'NoEvent'.
    fail _          = NoEvent


-- | Alternative instance
instance Alternative Event where
    -- | An empty alternative carries no event, so it is ignored.
    empty = NoEvent
    -- | Merge favouring the left event ('NoEvent' only if both are
    -- 'NoEvent').
    NoEvent <|> r = r
    l       <|> _ = l


-- | Forceable instance
instance Forceable a => Forceable (Event a) where
    -- | Force an event by evaluating its argument.
    force ea@NoEvent   = ea
    force ea@(Event a) = force a `seq` ea

-- | NFData instance
instance NFData a => NFData (Event a) where
    -- | Evaluate value carried by event.
    rnf NoEvent   = ()
    rnf (Event a) = rnf a `seq` ()

------------------------------------------------------------------------------
-- Internal utilities for event construction
------------------------------------------------------------------------------

-- These utilities are to be considered strictly internal to AFRP for the
-- time being.

-- | Convert a maybe value into a event ('Event' is isomorphic to 'Maybe').
maybeToEvent :: Maybe a -> Event a
maybeToEvent Nothing  = NoEvent
maybeToEvent (Just a) = Event a


------------------------------------------------------------------------------
-- Utility functions similar to those available for Maybe
------------------------------------------------------------------------------

-- | An event-based version of the maybe function.
event :: a -> (b -> a) -> Event b -> a
event a _ NoEvent   = a
event _ f (Event b) = f b

-- | Extract the value from an event. Fails if there is no event.
fromEvent :: Event a -> a
fromEvent (Event a) = a
fromEvent NoEvent   = usrErr "AFRP" "fromEvent" "Not an event."

-- | Tests whether the input represents an actual event.
isEvent :: Event a -> Bool
isEvent NoEvent   = False
isEvent (Event _) = True

-- | Negation of 'isEvent'.
isNoEvent :: Event a -> Bool
isNoEvent = not . isEvent


------------------------------------------------------------------------------
-- Event tagging
------------------------------------------------------------------------------

-- | Tags an (occurring) event with a value ("replacing" the old value).
--
-- Applicative-based definition:
--  tag = ($>)
tag :: Event a -> b -> Event b
e `tag` b = fmap (const b) e

-- | Tags an (occurring) event with a value ("replacing" the old value). Same
-- as 'tag' with the arguments swapped.
--
-- Applicative-based definition:
-- tagWith = (<$)
tagWith :: b -> Event a -> Event b
tagWith = flip tag

-- | Attaches an extra value to the value of an occurring event.
attach :: Event a -> b -> Event (a, b)
e `attach` b = fmap (\a -> (a, b)) e


------------------------------------------------------------------------------
-- Event merging (disjunction) and joining (conjunction)
------------------------------------------------------------------------------

-- !!! I think this is too complicated. rMerge can be obtained simply by
-- !!! swapping the arguments. So the only time it is possibly of any
-- !!! interest is for partial app. "merge" is inherently dangerous.
-- !!! But this is NOT obvious from its type: it's type is just like
-- !!! the others. This is the only example of such a def.
-- !!! Finally: mergeEvents is left-biased, but this is not reflected in
-- !!! its name.

-- | Left-biased event merge (always prefer left event, if present).
lMerge :: Event a -> Event a -> Event a
lMerge = (<|>)


-- | Right-biased event merge (always prefer right event, if present).
rMerge :: Event a -> Event a -> Event a
rMerge = flip (<|>)


-- | Unbiased event merge: simultaneous occurrence is an error.
merge :: Event a -> Event a -> Event a
merge = mergeBy (usrErr "AFRP" "merge" "Simultaneous event occurrence.")


-- | Event merge parameterized by a conflict resolution function.
--
-- Applicative-based definition:
-- mergeBy f re le = (f <$> re <*> le) <|> re <|> le
mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeBy _       NoEvent      NoEvent      = NoEvent
mergeBy _       le@(Event _) NoEvent      = le
mergeBy _       NoEvent      re@(Event _) = re
mergeBy resolve (Event l)    (Event r)    = Event (resolve l r)

-- | A generic event merge-map utility that maps event occurrences,
-- merging the results. The first three arguments are mapping functions,
-- the third of which will only be used when both events are present.
-- Therefore, 'mergeBy' = 'mapMerge' 'id' 'id'
mapMerge :: (a -> c) -> (b -> c) -> (a -> b -> c)
            -> Event a -> Event b -> Event c
mapMerge _  _  _   NoEvent   NoEvent   = NoEvent
mapMerge lf _  _   (Event l) NoEvent   = Event (lf l)
mapMerge _  rf _   NoEvent   (Event r) = Event (rf r)
mapMerge _  _  lrf (Event l) (Event r) = Event (lrf l r)

-- | Merge a list of events; foremost event has priority.
mergeEvents :: [Event a] -> Event a
mergeEvents = foldr lMerge NoEvent

-- | Collect simultaneous event occurrences; no event if none.
catEvents :: [Event a] -> Event [a]
catEvents eas = case [ a | Event a <- eas ] of
                    [] -> NoEvent
                    as -> Event as

-- | Join (conjunction) of two events. Only produces an event
-- if both events exist.
joinE :: Event a -> Event b -> Event (a,b)
joinE NoEvent   _         = NoEvent
joinE _         NoEvent   = NoEvent
joinE (Event l) (Event r) = Event (l,r)


-- | Split event carrying pairs into two events.
splitE :: Event (a,b) -> (Event a, Event b)
splitE NoEvent       = (NoEvent, NoEvent)
splitE (Event (a,b)) = (Event a, Event b)


------------------------------------------------------------------------------
-- Event filtering
------------------------------------------------------------------------------

-- | Filter out events that don't satisfy some predicate.
filterE :: (a -> Bool) -> Event a -> Event a
filterE p e@(Event a) = if p a then e else NoEvent
filterE _ NoEvent     = NoEvent


-- | Combined event mapping and filtering. Note: since 'Event' is a 'Functor',
-- see 'fmap' for a simpler version of this function with no filtering.
mapFilterE :: (a -> Maybe b) -> Event a -> Event b
mapFilterE _ NoEvent   = NoEvent
mapFilterE f (Event a) = case f a of
                            Nothing -> NoEvent
                            Just b  -> Event b


-- | Enable/disable event occurences based on an external condition.
gate :: Event a -> Bool -> Event a
_ `gate` False = NoEvent
e `gate` True  = e
