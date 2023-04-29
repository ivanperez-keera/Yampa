-- |
-- Module      : FRP.Yampa.Delays
-- Copyright   : (c) Ivan Perez, 2014-2022
--               (c) George Giorgidze, 2007-2012
--               (c) Henrik Nilsson, 2005-2006
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- SF primitives and combinators to delay signals, introducing new values in
-- them.
module FRP.Yampa.Delays
    (
      -- * Basic delays
      pre
    , iPre
    , fby

      -- * Timed delays
    , delay
    )
  where

-- External imports
import Control.Arrow ((>>>))

-- Internal imports
import FRP.Yampa.Basic        (identity, (-->))
import FRP.Yampa.Diagnostics  (usrErr)
import FRP.Yampa.InternalCore (SF (..), SF' (..), Time)
import FRP.Yampa.Scan         (sscanPrim)

infixr 0 `fby`

-- * Delays

-- | Uninitialized delay operator.
--
-- The output has an infinitesimal delay (1 sample), and the value at time zero
-- is undefined.
pre :: SF a a
pre = sscanPrim f uninit uninit
  where
    f c a = Just (a, c)
    uninit = usrErr "Yampa" "pre" "Uninitialized pre operator."

-- | Initialized delay operator.
--
-- Creates an SF that delays the input signal, introducing an infinitesimal
-- delay (one sample), using the given argument to fill in the initial output at
-- time zero.
iPre :: a -> SF a a
iPre = (--> pre)

-- | Lucid-Synchrone-like initialized delay (read "followed by").
--
-- Initialized delay combinator, introducing an infinitesimal delay (one sample)
-- in given 'SF', using the given argument to fill in the initial output at time
-- zero.
--
-- The difference with 'iPre' is that 'fby' takes an 'SF' as argument.
fby :: b -> SF a b -> SF a b
b0 `fby` sf = b0 --> sf >>> pre

-- * Timed delays

-- | Delay a signal by a fixed time 't', using the second parameter to fill in
-- the initial 't' seconds.
delay :: Time -> a -> SF a a
delay q aInit | q < 0     = usrErr "Yampa" "delay" "Negative delay."
              | q == 0    = identity
              | otherwise = SF {sfTF = tf0}
  where
    tf0 a0 = (delayAux [] [(q, a0)] 0 aInit, aInit)

    -- Invariants:
    -- tDiff measure the time since the latest output sample ideally should have
    -- been output. Whenever that equals or exceeds the time delta for the next
    -- buffered sample, it is time to output a new sample (although not
    -- necessarily the one first in the queue: it might be necessary to "catch
    -- up" by discarding samples.  0 <= tDiff < bdt, where bdt is the buffered
    -- time delta for the sample on the front of the buffer queue.
    --
    -- Sum of time deltas in the queue >= q.
    delayAux _ [] _ _ = undefined
    delayAux rbuf buf@((bdt, ba) : buf') tDiff aPrev = SF' tf -- True
      where
        tf dt a | tDiff' < bdt = (delayAux rbuf' buf tDiff' aPrev, aPrev)
                | otherwise    = nextSmpl rbuf' buf' (tDiff' - bdt) ba
          where
            tDiff' = tDiff + dt
            rbuf'  = (dt, a) : rbuf

            nextSmpl rbuf [] tDiff a =
              nextSmpl [] (reverse rbuf) tDiff a
            nextSmpl rbuf buf@((bdt, ba) : buf') tDiff a
              | tDiff < bdt = (delayAux rbuf buf tDiff a, a)
              | otherwise   = nextSmpl rbuf buf' (tDiff - bdt) ba
