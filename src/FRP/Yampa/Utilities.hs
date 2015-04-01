-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Utilities
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- Derived utility definitions.
--
-- ToDo:
--
-- * Possibly add
--       impulse :: VectorSpace a k => a -> Event a
--   But to do that, we need access to Event, which we currently do not have.
--
-- * The general arrow utilities should be moved to a module
--   FRP.Yampa.Utilities.
--
-- * I'm not sure structuring the Yampa \"core\" according to what is
--   core functionality and what's not is all that useful. There are
--   many cases where we want to implement combinators that fairly
--   easily could be implemented in terms of others as primitives simply
--   because we expect that that implementation is going to be much more
--   efficient, and that the combinators are used sufficiently often to
--   warrant doing this. E.g. 'switch' should be a primitive, even though
--   it could be derived from 'pSwitch'.
--
-- * Reconsider 'recur'. If an event source has an immediate occurrence,
--   we'll get into a loop. For example: recur now. Maybe suppress
--   initial occurrences? Initial occurrences are rather pointless in this
--   case anyway.
-----------------------------------------------------------------------------------------

module FRP.Yampa.Utilities (
-- Now defined in Control.Arrow
-- General arrow utilities
    (^>>),      -- :: Arrow a => (b -> c) -> a c d -> a b d
    (>>^),      -- :: Arrow a => a b c -> (c -> d) -> a b d
    (^<<),      -- :: Arrow a => (c -> d) -> a b c -> a b d
    (<<^),      -- :: Arrow a => a c d -> (b -> c) -> a b d

-- Liftings
    arr2,       -- :: Arrow a => (b->c->d) -> a (b,c) d
    arr3,       -- :: Arrow a => (b->c->d->e) -> a (b,c,d) e
    arr4,       -- :: Arrow a => (b->c->d->e->f) -> a (b,c,d,e) f
    arr5,       -- :: Arrow a => (b->c->d->e->f->g) -> a (b,c,d,e,f) g
    lift0,      -- :: Arrow a => c -> a b c
    lift1,      -- :: Arrow a => (c->d) -> (a b c->a b d)
    lift2,      -- :: Arrow a => (c->d->e) -> (a b c->a b d->a b e)
    lift3,      -- :: Arrow a => (c->d->e->f) -> (a b c-> ... ->a b f)
    lift4,      -- :: Arrow a => (c->d->e->f->g) -> (a b c->...->a b g)
    lift5,      -- :: Arrow a => (c->d->e->f->g->h)->(a b c->...a b h)


-- Wave-form generation
    old_dHold,      -- :: a -> SF (Event a) a

-- Accumulators
    old_accumHold,    -- :: a -> SF (Event (a -> a)) a
    old_dAccumHold,   -- :: a -> SF (Event (a -> a)) a
    old_accumHoldBy,  -- :: (b -> a -> b) -> b -> SF (Event a) b
    old_dAccumHoldBy, -- :: (b -> a -> b) -> b -> SF (Event a) b

-- Integrals
    old_impulseIntegral -- :: VectorSpace a k => SF (a, Event a) a

    , sampleWindow
) where

import FRP.Yampa.Diagnostics
import FRP.Yampa



-- Now defined directly in Control.Arrow.
-- But while using an old version of Arrows ...
------------------------------------------------------------------------------
-- General arrow utilities
------------------------------------------------------------------------------
{-
(^>>) :: Arrow a => (b -> c) -> a c d -> a b d
f ^>> a = arr f >>> a

(>>^) :: Arrow a => a b c -> (c -> d) -> a b d
a >>^ f = a >>> arr f


(^<<) :: Arrow a => (c -> d) -> a b c -> a b d
f ^<< a = arr f <<< a


(<<^) :: Arrow a => a c d -> (b -> c) -> a b d
a <<^ f = a <<< arr f
-}

------------------------------------------------------------------------------
-- Liftings
------------------------------------------------------------------------------

arr2 :: Arrow a => (b -> c -> d) -> a (b, c) d
arr2 = arr . uncurry


arr3 :: Arrow a => (b -> c -> d -> e) -> a (b, c, d) e
arr3 = arr . \h (b, c, d) -> h b c d


arr4 :: Arrow a => (b -> c -> d -> e -> f) -> a (b, c, d, e) f
arr4 = arr . \h (b, c, d, e) -> h b c d e


arr5 :: Arrow a => (b -> c -> d -> e -> f -> g) -> a (b, c, d, e, f) g
arr5 = arr . \h (b, c, d, e, f) -> h b c d e f


lift0 :: Arrow a => c -> a b c
lift0 c = arr (const c)


lift1 :: Arrow a => (c -> d) -> (a b c -> a b d)
lift1 f = \a -> a >>> arr f


lift2 :: Arrow a => (c -> d -> e) -> (a b c -> a b d -> a b e)
lift2 f = \a1 a2 -> a1 &&& a2 >>> arr2 f


lift3 :: Arrow a => (c -> d -> e -> f) -> (a b c -> a b d -> a b e -> a b f)
lift3 f = \a1 a2 a3 -> (lift2 f) a1 a2 &&& a3 >>> arr2 ($)


lift4 :: Arrow a => (c->d->e->f->g) -> (a b c->a b d->a b e->a b f->a b g)
lift4 f = \a1 a2 a3 a4 -> (lift3 f) a1 a2 a3 &&& a4 >>> arr2 ($)


lift5 :: Arrow a =>
    (c->d->e->f->g->h) -> (a b c->a b d->a b e->a b f->a b g->a b h)
lift5 f = \a1 a2 a3 a4 a5 ->(lift4 f) a1 a2 a3 a4 &&& a5 >>> arr2 ($)


------------------------------------------------------------------------------
-- Event sources
------------------------------------------------------------------------------




------------------------------------------------------------------------------
-- Wave-form generation
------------------------------------------------------------------------------

-- Zero-order hold with delay.
-- Identity: dHold a0 = hold a0 >>> iPre a0).
old_dHold :: a -> SF (Event a) a
old_dHold a0 = dSwitch (constant a0 &&& identity) dHold'
    where
    dHold' a = dSwitch (constant a &&& notYet) dHold'



------------------------------------------------------------------------------
-- Accumulators
------------------------------------------------------------------------------

{-# DEPRECATED old_accumHold "Use accumHold instead" #-}
old_accumHold :: a -> SF (Event (a -> a)) a
old_accumHold a_init = old_accum a_init >>> old_hold a_init


{-# DEPRECATED old_dAccumHold "Use dAccumHold instead" #-}
old_dAccumHold :: a -> SF (Event (a -> a)) a
old_dAccumHold a_init = old_accum a_init >>> old_dHold a_init


{-# DEPRECATED old_accumHoldBy "Use accumHoldBy instead" #-}
old_accumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
old_accumHoldBy f b_init = old_accumBy f b_init >>> old_hold b_init


{-# DEPRECATED old_dAccumHoldBy "Use dAccumHoldBy instead" #-}
old_dAccumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
old_dAccumHoldBy f b_init = old_accumBy f b_init >>> old_dHold b_init

------------------------------------------------------------------------------
-- Integrals
------------------------------------------------------------------------------

old_impulseIntegral :: VectorSpace a k => SF (a, Event a) a
old_impulseIntegral = (integral *** old_accumHoldBy (^+^) zeroVector) >>^ uncurry (^+^)

-- * Window sampling
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
            where
            w' = w ++ as

