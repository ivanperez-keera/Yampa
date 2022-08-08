-- |
-- Module      :  FRP.Yampa.Delays
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
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

import Control.Arrow

import FRP.Yampa.Basic
import FRP.Yampa.Diagnostics
import FRP.Yampa.InternalCore (SF (..), SF' (..), Time)
import FRP.Yampa.Scan

infixr 0 `fby`

-- * Delays

-- | Uninitialized delay operator.
--
-- The output has an infinitesimal delay (1 sample), and the value at time
-- zero is undefined.
pre :: SF a a
pre = sscanPrim f uninit uninit
  where
    f c a = Just (a, c)
    uninit = usrErr "Yampa" "pre" "Uninitialized pre operator."

-- | Initialized delay operator.
--
-- Creates an SF that delays the input signal, introducing an infinitesimal
-- delay (one sample), using the given argument to fill in the initial output
-- at time zero.

iPre :: a -> SF a a
iPre = (--> pre)

-- | Lucid-Synchrone-like initialized delay (read "followed by").
--
-- Initialized delay combinator, introducing an infinitesimal delay (one
-- sample) in given 'SF', using the given argument to fill in the initial
-- output at time zero.
--
-- The difference with 'iPre' is that 'fby' takes an 'SF' as argument.
fby :: b -> SF a b -> SF a b
b0 `fby` sf = b0 --> sf >>> pre

-- * Timed delays

-- | Delay a signal by a fixed time 't', using the second parameter
-- to fill in the initial 't' seconds.
delay :: Time -> a -> SF a a
delay q a_init | q < 0     = usrErr "Yampa" "delay" "Negative delay."
               | q == 0    = identity
               | otherwise = SF {sfTF = tf0}
  where
    tf0 a0 = (delayAux [] [(q, a0)] 0 a_init, a_init)

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
