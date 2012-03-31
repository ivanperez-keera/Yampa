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
    (^>>),		-- :: Arrow a => (b -> c) -> a c d -> a b d
    (>>^),		-- :: Arrow a => a b c -> (c -> d) -> a b d
    (^<<),		-- :: Arrow a => (c -> d) -> a b c -> a b d 
    (<<^),		-- :: Arrow a => a c d -> (b -> c) -> a b d

-- Liftings
    arr2,		-- :: Arrow a => (b->c->d) -> a (b,c) d
    arr3,		-- :: Arrow a => (b->c->d->e) -> a (b,c,d) e
    arr4,		-- :: Arrow a => (b->c->d->e->f) -> a (b,c,d,e) f
    arr5,		-- :: Arrow a => (b->c->d->e->f->g) -> a (b,c,d,e,f) g
    lift0,		-- :: Arrow a => c -> a b c
    lift1,		-- :: Arrow a => (c->d) -> (a b c->a b d)
    lift2,		-- :: Arrow a => (c->d->e) -> (a b c->a b d->a b e)
    lift3,		-- :: Arrow a => (c->d->e->f) -> (a b c-> ... ->a b f)
    lift4,		-- :: Arrow a => (c->d->e->f->g) -> (a b c->...->a b g)
    lift5,		-- :: Arrow a => (c->d->e->f->g->h)->(a b c->...a b h)

-- Event sources
    snap,		-- :: SF a (Event a)
    snapAfter,		-- :: Time -> SF a (Event a)
    sample,		-- :: Time -> SF a (Event a)
    recur,		-- :: SF a (Event b) -> SF a (Event b)
    andThen,            -- :: SF a (Event b)->SF a (Event b)->SF a (Event b)
    sampleWindow,	-- :: Int -> Time -> SF a (Event [a])

-- Parallel composition/switchers with "zip" routing
    parZ,		-- [SF a b] -> SF [a] [b]
    pSwitchZ,		-- [SF a b] -> SF ([a],[b]) (Event c)
			-- -> ([SF a b] -> c -> SF [a] [b]) -> SF [a] [b]
    dpSwitchZ,		-- [SF a b] -> SF ([a],[b]) (Event c)
			-- -> ([SF a b] -> c ->SF [a] [b]) -> SF [a] [b]
    rpSwitchZ,		-- [SF a b] -> SF ([a], Event ([SF a b]->[SF a b])) [b]
    drpSwitchZ,		-- [SF a b] -> SF ([a], Event ([SF a b]->[SF a b])) [b]

-- Guards and automata-oriented combinators
    provided,		-- :: (a -> Bool) -> SF a b -> SF a b -> SF a b

-- Wave-form generation
    old_dHold,		-- :: a -> SF (Event a) a
    dTrackAndHold,	-- :: a -> SF (Maybe a) a

-- Accumulators
    old_accumHold,	-- :: a -> SF (Event (a -> a)) a
    old_dAccumHold,	-- :: a -> SF (Event (a -> a)) a
    old_accumHoldBy,	-- :: (b -> a -> b) -> b -> SF (Event a) b
    old_dAccumHoldBy,	-- :: (b -> a -> b) -> b -> SF (Event a) b
    count,		-- :: Integral b => SF (Event a) (Event b)

-- Delays
    fby,		-- :: b -> SF a b -> SF a b,	infixr 0

-- Integrals
    impulseIntegral,	-- :: VectorSpace a k => SF (a, Event a) a
    old_impulseIntegral	-- :: VectorSpace a k => SF (a, Event a) a
) where

import FRP.Yampa.Diagnostics
import FRP.Yampa


infixr 5 `andThen`
--infixr 1 ^<<, ^>>
--infixr 1 <<^, >>^
infixr 0 `fby`


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

-- Window sampling
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


------------------------------------------------------------------------------
-- Parallel composition/switchers with "zip" routing
------------------------------------------------------------------------------

safeZip :: String -> [a] -> [b] -> [(a,b)]
safeZip fn as bs = safeZip' as bs
    where
	safeZip' _  []     = []
	safeZip' as (b:bs) = (head' as, b) : safeZip' (tail' as) bs

	head' []    = err
	head' (a:_) = a

	tail' []     = err
	tail' (_:as) = as

	err = usrErr "AFRPUtilities" fn "Input list too short."


parZ :: [SF a b] -> SF [a] [b]
parZ = par (safeZip "parZ")


pSwitchZ :: [SF a b] -> SF ([a],[b]) (Event c) -> ([SF a b] -> c -> SF [a] [b])
            -> SF [a] [b]
pSwitchZ = pSwitch (safeZip "pSwitchZ")


dpSwitchZ :: [SF a b] -> SF ([a],[b]) (Event c) -> ([SF a b] -> c ->SF [a] [b])
             -> SF [a] [b]
dpSwitchZ = dpSwitch (safeZip "dpSwitchZ")


rpSwitchZ :: [SF a b] -> SF ([a], Event ([SF a b] -> [SF a b])) [b]
rpSwitchZ = rpSwitch (safeZip "rpSwitchZ")


drpSwitchZ :: [SF a b] -> SF ([a], Event ([SF a b] -> [SF a b])) [b]
drpSwitchZ = drpSwitch (safeZip "drpSwitchZ")


------------------------------------------------------------------------------
-- Guards and automata-oriented combinators
------------------------------------------------------------------------------

-- Runs sft only when the predicate p is satisfied, otherwise runs sff.
provided :: (a -> Bool) -> SF a b -> SF a b -> SF a b
provided p sft sff =
    switch (constant undefined &&& snap) $ \a0 ->
    if p a0 then stt else stf
    where
	stt = switch (sft &&& (not . p ^>> edge)) (const stf)
        stf = switch (sff &&& (p ^>> edge)) (const stt)


------------------------------------------------------------------------------
-- Wave-form generation
------------------------------------------------------------------------------

-- Zero-order hold with delay.
-- Identity: dHold a0 = hold a0 >>> iPre a0).
old_dHold :: a -> SF (Event a) a
old_dHold a0 = dSwitch (constant a0 &&& identity) dHold'
    where
	dHold' a = dSwitch (constant a &&& notYet) dHold'


dTrackAndHold :: a -> SF (Maybe a) a
dTrackAndHold a_init = trackAndHold a_init >>> iPre a_init


------------------------------------------------------------------------------
-- Accumulators
------------------------------------------------------------------------------

old_accumHold :: a -> SF (Event (a -> a)) a
old_accumHold a_init = old_accum a_init >>> old_hold a_init


old_dAccumHold :: a -> SF (Event (a -> a)) a
old_dAccumHold a_init = old_accum a_init >>> old_dHold a_init


old_accumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
old_accumHoldBy f b_init = old_accumBy f b_init >>> old_hold b_init


old_dAccumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
old_dAccumHoldBy f b_init = old_accumBy f b_init >>> old_dHold b_init


count :: Integral b => SF (Event a) (Event b)
count = accumBy (\n _ -> n + 1) 0


------------------------------------------------------------------------------
-- Delays
------------------------------------------------------------------------------

-- Lucid-Synchrone-like initialized delay (read "followed by").
fby :: b -> SF a b -> SF a b
b0 `fby` sf = b0 --> sf >>> pre


------------------------------------------------------------------------------
-- Integrals
------------------------------------------------------------------------------

impulseIntegral :: VectorSpace a k => SF (a, Event a) a
impulseIntegral = (integral *** accumHoldBy (^+^) zeroVector) >>^ uncurry (^+^)

old_impulseIntegral :: VectorSpace a k => SF (a, Event a) a
old_impulseIntegral = (integral *** old_accumHoldBy (^+^) zeroVector) >>^ uncurry (^+^)
