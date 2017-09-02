-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Hybrid
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.Hybrid (


-- * Discrete to continuous-time signal functions
-- ** Wave-form generation
    hold,               -- :: a -> SF (Event a) a
    dHold,              -- :: a -> SF (Event a) a
    trackAndHold,       -- :: a -> SF (Maybe a) a
    dTrackAndHold,      -- :: a -> SF (Maybe a) a

-- ** Accumulators
    accum,              -- :: a -> SF (Event (a -> a)) (Event a)
    accumHold,          -- :: a -> SF (Event (a -> a)) a
    dAccumHold,         -- :: a -> SF (Event (a -> a)) a
    accumBy,            -- :: (b -> a -> b) -> b -> SF (Event a) (Event b)
    accumHoldBy,        -- :: (b -> a -> b) -> b -> SF (Event a) b
    dAccumHoldBy,       -- :: (b -> a -> b) -> b -> SF (Event a) b
    accumFilter,        -- :: (c -> a -> (c, Maybe b)) -> c
                        --    -> SF (Event a) (Event b)

) where

import Control.Arrow

import FRP.Yampa.InternalCore (SF, epPrim)

import FRP.Yampa.Delays
import FRP.Yampa.Event

------------------------------------------------------------------------------
-- Wave-form generation
------------------------------------------------------------------------------

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

dTrackAndHold :: a -> SF (Maybe a) a
dTrackAndHold a_init = trackAndHold a_init >>> iPre a_init

------------------------------------------------------------------------------
-- Accumulators
------------------------------------------------------------------------------

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



-- Vim modeline
-- vim:set tabstop=8 expandtab:
