{-# LANGUAGE GADTs, Rank2Types, CPP #-}
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
    pre,		-- :: SF a a
    iPre,		-- :: a -> SF a a
    old_pre, old_iPre,

-- ** Timed delays
    delay,		-- :: Time -> a -> SF a a

-- ** Variable delay
    pause,              -- :: b -> SF a b -> SF a Bool -> SF a b

) where


import FRP.Yampa.Diagnostics
import FRP.Yampa.InternalCore

import FRP.Yampa.Basic
import FRP.Yampa.Scan

------------------------------------------------------------------------------
-- Delays
------------------------------------------------------------------------------

-- | Uninitialized delay operator (old implementation).

-- !!! The seq helps in the dynamic delay line example. But is it a good
-- !!! idea in general? Are there other accumulators which should be seq'ed
-- !!! as well? E.g. accum? Switch? Anywhere else? What's the underlying
-- !!! design principle? What can the user assume?
--
old_pre :: SF a a
old_pre = SF {sfTF = tf0}
    where
        tf0 a0 = (preAux a0, usrErr "AFRP" "pre" "Uninitialized pre operator.")

	preAux a_prev = SF' tf -- True
	    where
		tf _ a = {- a_prev `seq` -} (preAux a, a_prev)

-- | Initialized delay operator (old implementation).
old_iPre :: a -> SF a a
old_iPre = (--> old_pre)



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


------------------------------------------------------------------------------
-- Variable pause in signal
------------------------------------------------------------------------------

-- | Given a value in an accumulator (b), a predicate signal function (sfC), 
--   and a second signal function (sf), pause will produce the accumulator b
--   if sfC input is True, and will transform the signal using sf otherwise.
--   It acts as a pause with an accumulator for the moments when the
--   transformation is paused.
pause :: b -> SF a Bool -> SF a b -> SF a b
pause b_init (SF { sfTF = tfP}) (SF {sfTF = tf10}) = SF {sfTF = tf0}
 where
       -- Initial transformation (no time delta):
       -- If the condition is True, return the accumulator b_init)
       -- Otherwise transform the input normally and recurse.
       tf0 a0 = case tfP a0 of
                 (c, True)  -> (pauseInit b_init tf10 c, b_init)
                 (c, False) -> let (k, b0) = tf10 a0
                               in (pause' b0 k c, b0)

       -- Similar deal, but with a time delta
       pauseInit :: b -> (a -> Transition a b) -> SF' a Bool -> SF' a b
       pauseInit b_init' tf10' c = SF' tf0'
         where tf0' dt a =
                case (sfTF' c) dt a of
                  (c', True)  -> (pauseInit b_init' tf10' c', b_init')
                  (c', False) -> let (k, b0) = tf10' a
                                 in (pause' b0 k c', b0)

       -- Very same deal (almost alpha-renameable)
       pause' :: b -> SF' a b -> SF' a Bool -> SF' a b
       pause' b_init' tf10' tfP' = SF' tf0'
         where tf0' dt a = 
                 case (sfTF' tfP') dt a of
                   (tfP'', True) -> (pause' b_init' tf10' tfP'', b_init')
                   (tfP'', False) -> let (tf10'', b0') = (sfTF' tf10') dt a
                                     in (pause' b0' tf10'' tfP'', b0')

-- if_then_else :: SF a Bool -> SF a b -> SF a b -> SF a b
-- if_then_else condSF sfThen sfElse = proc (i) -> do
--   cond  <- condSF -< i
--   ok    <- sfThen -< i
--   notOk <- sfElse -< i
--   returnA -< if cond then ok else notOk

-- Vim modeline
-- vim:set tabstop=8 expandtab:
