-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Delays
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.Delays (

    -- * Delays
    -- ** Basic delays
    pre,                -- :: SF a a
    iPre,               -- :: a -> SF a a

    -- ** Timed delays
    delay,              -- :: Time -> a -> SF a a

    -- ** To be completed
    fby,        -- :: b -> SF a b -> SF a b,    infixr 0
) where

import Control.Arrow

import FRP.Yampa.Diagnostics
import FRP.Yampa.InternalCore (SF(..), SF'(..), Time)

import FRP.Yampa.Basic
import FRP.Yampa.Scan

infixr 0 `fby`

------------------------------------------------------------------------------
-- Delays
------------------------------------------------------------------------------

-- | Uninitialized delay operator.

-- !!! Redefined using SFSScan
-- !!! About 20% slower than old_pre on its own.
pre :: SF a a
pre = sscanPrim f uninit uninit
    where
        f c a = Just (a, c)
        uninit = usrErr "AFRP" "pre" "Uninitialized pre operator."


-- | Initialized delay operator.
iPre :: a -> SF a a
iPre = (--> pre)


------------------------------------------------------------------------------
-- Timed delays
------------------------------------------------------------------------------

-- | Delay a signal by a fixed time 't', using the second parameter
-- to fill in the initial 't' seconds.

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

-- !!! PROBLEM!
-- Since input samples sometimes need to be duplicated, it is not a
-- good idea use a delay on things like events since we then could
-- end up with duplication of event occurrences.
-- (Thus, we actually NEED delayEvent.)

delay :: Time -> a -> SF a a
delay q a_init | q < 0     = usrErr "AFRP" "delay" "Negative delay."
               | q == 0    = identity
               | otherwise = SF {sfTF = tf0}
    where
        tf0 a0 = (delayAux [] [(q, a0)] 0 a_init, a_init)

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


-- !!! Hmm. Not so easy to do efficiently, it seems ...

-- varDelay :: Time -> a -> SF (a, Time) a
-- varDelay = undefined


-- if_then_else :: SF a Bool -> SF a b -> SF a b -> SF a b
-- if_then_else condSF sfThen sfElse = proc (i) -> do
--   cond  <- condSF -< i
--   ok    <- sfThen -< i
--   notOk <- sfElse -< i
--   returnA -< if cond then ok else notOk

-- | Lucid-Synchrone-like initialized delay (read "followed by").
fby :: b -> SF a b -> SF a b
b0 `fby` sf = b0 --> sf >>> pre


-- Vim modeline
-- vim:set tabstop=8 expandtab:
