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

module FRP.Yampa.Utilities (sampleWindow) where

import Control.Arrow

import FRP.Yampa.Basic
import FRP.Yampa.Core
import FRP.Yampa.EventS
import FRP.Yampa.Hybrid

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
