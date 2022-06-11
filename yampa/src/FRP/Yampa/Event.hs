{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : FRP.Yampa.Event
-- Copyright   : (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
-- Portability : portable
--
-- Events in Yampa represent discrete time-signals, meaning those that do not
-- change continuously. Examples of event-carrying signals would be mouse
-- clicks (in between clicks it is assumed that there is no click), some
-- keyboard events, button presses on wiimotes or window-manager events.
--
-- The type 'Event' is isomorphic to 'Maybe' (@Event a = NoEvent | Event a@)
-- but, semantically, a 'Maybe'-carrying signal could change continuously,
-- whereas an 'Event'-carrying signal should not: for two events in subsequent
-- samples, there should be an small enough sampling frequency such that we
-- sample between those two samples and there are no 'Event's between them.
-- Nevertheless, no mechanism in Yampa will check this or misbehave if this
-- assumption is violated.
--
-- Events are essential for many other Yampa constructs, like switches (see
-- 'FRP.Yampa.Switches.switch' for details).
module FRP.Yampa.Event where

import           Control.Applicative
import           Control.DeepSeq     (NFData (..))
import qualified Control.Monad.Fail  as Fail

import FRP.Yampa.Diagnostics

infixl 8 `tag`, `attach`, `gate`
infixl 7 `joinE`
infixl 6 `lMerge`, `rMerge`, `merge`

-- * The Event type

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

#if !(MIN_VERSION_base(4,13,0))
  -- | Fail with 'NoEvent'.
  fail            = Fail.fail
#endif

instance Fail.MonadFail Event where
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

-- | NFData instance
instance NFData a => NFData (Event a) where
  -- | Evaluate value carried by event.
  rnf NoEvent   = ()
  rnf (Event a) = rnf a `seq` ()

-- * Internal utilities for event construction

-- These utilities are to be considered strictly internal to Yampa for the
-- time being.

-- | Convert a maybe value into a event ('Event' is isomorphic to 'Maybe').
maybeToEvent :: Maybe a -> Event a
maybeToEvent Nothing  = NoEvent
maybeToEvent (Just a) = Event a

-- * Utility functions similar to those available for Maybe

-- | An event-based version of the maybe function.
event :: a -> (b -> a) -> Event b -> a
event a _ NoEvent   = a
event _ f (Event b) = f b

-- | Extract the value from an event. Fails if there is no event.
fromEvent :: Event a -> a
fromEvent (Event a) = a
fromEvent NoEvent   = usrErr "Yampa" "fromEvent" "Not an event."

-- | Tests whether the input represents an actual event.
isEvent :: Event a -> Bool
isEvent NoEvent   = False
isEvent (Event _) = True

-- | Negation of 'isEvent'.
isNoEvent :: Event a -> Bool
isNoEvent = not . isEvent

-- * Event tagging

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

-- * Event merging (disjunction) and joining (conjunction)

-- | Left-biased event merge (always prefer left event, if present).
lMerge :: Event a -> Event a -> Event a
lMerge = (<|>)

-- | Right-biased event merge (always prefer right event, if present).
rMerge :: Event a -> Event a -> Event a
rMerge = flip (<|>)

-- | Unbiased event merge: simultaneous occurrence is an error.
merge :: Event a -> Event a -> Event a
merge = mergeBy (usrErr "Yampa" "merge" "Simultaneous event occurrence.")

-- | Event merge parameterized by a conflict resolution function.
--
-- Applicative-based definition:
-- mergeBy f le re = (f <$> le <*> re) <|> le <|> re
mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeBy _       NoEvent      NoEvent      = NoEvent
mergeBy _       le@(Event _) NoEvent      = le
mergeBy _       NoEvent      re@(Event _) = re
mergeBy resolve (Event l)    (Event r)    = Event (resolve l r)

-- | A generic event merge-map utility that maps event occurrences,
-- merging the results. The first three arguments are mapping functions,
-- the third of which will only be used when both events are present.
-- Therefore, 'mergeBy' = 'mapMerge' 'id' 'id'
--
-- Applicative-based definition:
-- mapMerge lf rf lrf le re = (f <$> le <*> re) <|> (lf <$> le) <|> (rf <$> re)
mapMerge :: (a -> c) -> (b -> c) -> (a -> b -> c)
            -> Event a -> Event b -> Event c
mapMerge _  _  _   NoEvent   NoEvent   = NoEvent
mapMerge lf _  _   (Event l) NoEvent   = Event (lf l)
mapMerge _  rf _   NoEvent   (Event r) = Event (rf r)
mapMerge _  _  lrf (Event l) (Event r) = Event (lrf l r)

-- | Merge a list of events; foremost event has priority.
--
-- Foldable-based definition:
-- mergeEvents :: Foldable t => t (Event a) -> Event a
-- mergeEvents =  asum
mergeEvents :: [Event a] -> Event a
mergeEvents = foldr lMerge NoEvent

-- | Collect simultaneous event occurrences; no event if none.
--
-- Traverable-based definition:
-- catEvents :: Foldable t => t (Event a) -> Event (t a)
-- catEvents e  = if (null e) then NoEvent else (sequenceA e)
catEvents :: [Event a] -> Event [a]
catEvents eas = case [ a | Event a <- eas ] of
                  [] -> NoEvent
                  as -> Event as

-- | Join (conjunction) of two events. Only produces an event
-- if both events exist.
--
-- Applicative-based definition:
-- joinE = liftA2 (,)
joinE :: Event a -> Event b -> Event (a,b)
joinE NoEvent   _         = NoEvent
joinE _         NoEvent   = NoEvent
joinE (Event l) (Event r) = Event (l,r)

-- | Split event carrying pairs into two events.
splitE :: Event (a,b) -> (Event a, Event b)
splitE NoEvent       = (NoEvent, NoEvent)
splitE (Event (a,b)) = (Event a, Event b)

-- * Event filtering

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

-- | Enable/disable event occurrences based on an external condition.
gate :: Event a -> Bool -> Event a
_ `gate` False = NoEvent
e `gate` True  = e
