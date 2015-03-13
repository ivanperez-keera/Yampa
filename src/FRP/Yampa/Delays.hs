{-# LANGUAGE GADTs, Rank2Types, CPP #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
-- 
--
-- Domain-specific language embedded in Haskell for programming hybrid (mixed
-- discrete-time and continuous-time) systems. Yampa is based on the concepts
-- of Functional Reactive Programming (FRP) and is structured using arrow
-- combinators.
--
-- You can find examples, tutorials and documentation on Yampa here:
--
-- <www.haskell.org/haskellwiki/Yampa>
--
-- Structuring a hybrid system in Yampa is done based on two main concepts:
--
-- * Signal Functions: 'SF'. Yampa is based on the concept of Signal Functions,
-- which are functions from a typed input signal to a typed output signal.
-- Conceptually, signals are functions from Time to Value, where time are the
-- real numbers and, computationally, a very dense approximation (Double) is
-- used.
--
-- * Events: 'Event'. Values that may or may not occur (and would probably
-- occur rarely). It is often used for incoming network messages, mouse
-- clicks, etc. Events are used as values carried by signals.
--
-- A complete Yampa system is defined as one Signal Function from some
-- type @a@ to a type @b@. The execution of this signal transformer
-- with specific input can be accomplished by means of two functions:
-- 'reactimate' (which needs an initialization action,
-- an input sensing action and an actuation/consumer action and executes
-- until explicitly stopped), and 'react' (which executes only one cycle).
-- 
-- Apart from using normal functions and arrow syntax to define 'SF's, you
-- can also use several combinators. See [<#g:4>] for basic signals combinators,
-- [<#g:11>] for ways of switching from one signal transformation to another,
-- and [<#g:16>] for ways of transforming Event-carrying signals into continuous
-- signals, [<#g:19>] for ways of delaying signals, and [<#g:21>] for ways to
-- feed a signal back to the same signal transformer.
--
-- Ways to define Event-carrying signals are given in [<#g:7>], and
-- "FRP.Yampa.Event" defines events and event-manipulation functions.
--
-- Finally, see [<#g:26>] for sources of randomness (useful in games).
--
-- CHANGELOG:
--
-- * Adds (most) documentation.
--
-- * New version using GADTs.
--
-- ToDo:
--
-- * Specialize def. of repeatedly. Could have an impact on invaders.
--
-- * New defs for accs using SFAcc
--
-- * Make sure opt worked: e.g.
--
--   >     repeatedly >>> count >>> arr (fmap sqr)
--
-- * Introduce SFAccHld.
--
-- * See if possible to unify AccHld wity Acc??? They are so close.
--
-- * Introduce SScan. BUT KEEP IN MIND: Most if not all opts would
--   have been possible without GADTs???
--
-- * Look into pairs. At least pairing of SScan ought to be interesting.
--
-- * Would be nice if we could get rid of first & second with impunity
--   thanks to Id optimizations. That's a clear win, with or without
--   an explicit pair combinator.
--
-- * delayEventCat is a bit complicated ...
--
--
-- Random ideas:
--
-- * What if one used rules to optimize
--   - (arr :: SF a ()) to (constant ())
--   - (arr :: SF a a) to identity
--   But inspection of invader source code seem to indicate that
--   these are not very common cases at all.
--
-- * It would be nice if it was possible to come up with opt. rules
--   that are invariant of how signal function expressions are
--   parenthesized. Right now, we have e.g.
--       arr f >>> (constant c >>> sf)
--   being optimized to
--       cpAuxA1 f (cpAuxC1 c sf)
--   whereas it clearly should be possible to optimize to just
--       cpAuxC1 c sf
--   What if we didn't use SF' but
--      SFComp :: <tfun> -> SF' a b -> SF' b c -> SF' a c
--   ???
--
-- * The transition function would still be optimized in (pretty much)
--   the current way, but it would still be possible to look "inside"
--   composed signal functions for lost optimization opts.
--   Seems to me this could be done without too much extra effort/no dupl.
--   work.
--   E.g. new cpAux, the general case:
--
-- @
--      cpAux sf1 sf2 = SFComp tf sf1 sf2
--          where
--              tf dt a = (cpAux sf1' sf2', c)
--                  where
--                      (sf1', b) = (sfTF' sf1) dt a
--                      (sf2', c) = (sfTF' sf2) dt b
-- @
--
-- * The ONLY change was changing the constructor from SF' to SFComp and
--   adding sf1 and sf2 to the constructor app.!
--
-- * An optimized case:
--     cpAuxC1 b sf1 sf2               = SFComp tf sf1 sf2
--   So cpAuxC1 gets an extra arg, and we change the constructor.
--   But how to exploit without writing 1000s of rules???
--   Maybe define predicates on SFComp to see if the first or second
--   sf are "interesting", and if so, make "reassociate" and make a
--   recursive call? E.g. we're in the arr case, and the first sf is another
--   arr, so we'd like to combine the two.
--
-- * It would also be intersting, then, to know when to STOP playing this
--   game, due to the overhead involved.
--
-- * Why don't we have a "SWITCH" constructor that indicates that the
--   structure will change, and thus that it is worthwile to keep
--   looking for opt. opportunities, whereas a plain "SF'" would
--   indicate that things NEVER are going to change, and thus we can just
--   as well give up?
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


import FRP.Yampa.Core
import FRP.Yampa.Diagnostics

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
