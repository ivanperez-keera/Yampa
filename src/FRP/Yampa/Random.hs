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

module FRP.Yampa.Random (
    RandomGen(..),
    Random(..),

-- * Noise (random signal) sources and stochastic event sources
    noise,		-- :: noise :: (RandomGen g, Random b) =>
			--        g -> SF a b
    noiseR,		-- :: noise :: (RandomGen g, Random b) =>
			--        (b,b) -> g -> SF a b
    occasionally,	-- :: RandomGen g => g -> Time -> b -> SF a (Event b)

) where

import System.Random (RandomGen(..), Random(..))

import FRP.Yampa.Core
import FRP.Yampa.Diagnostics
import FRP.Yampa.Event

------------------------------------------------------------------------------
-- Noise (i.e. random signal generators) and stochastic processes
------------------------------------------------------------------------------

-- | Noise (random signal) with default range for type in question;
-- based on "randoms".
noise :: (RandomGen g, Random b) => g -> SF a b
noise g0 = streamToSF (randoms g0)


-- | Noise (random signal) with specified range; based on "randomRs".
noiseR :: (RandomGen g, Random b) => (b,b) -> g -> SF a b
noiseR range g0 = streamToSF (randomRs range g0)


-- Internal. Not very useful for other purposes since we do not have any
-- control over the intervals between each "sample". Or? A version with
-- time-stamped samples would be similar to embedSynch (applied to identity).
-- The list argument must be a stream (infinite list) at present.

streamToSF :: [b] -> SF a b
streamToSF []     = intErr "AFRP" "streamToSF" "Empty list!"
streamToSF (b:bs) = SF {sfTF = tf0}
    where
        tf0 _ = (stsfAux bs, b)

        stsfAux []     = intErr "AFRP" "streamToSF" "Empty list!"
	-- Invarying since stsfAux [] is an error.
        stsfAux (b:bs) = SF' tf -- True
	    where
		tf _ _ = (stsfAux bs, b)

{- New def, untested:

streamToSF = sscan2 f
    where
        f []     _ = intErr "AFRP" "streamToSF" "Empty list!"
        f (b:bs) _ = (bs, b)

-}


-- | Stochastic event source with events occurring on average once every t_avg
-- seconds. However, no more than one event results from any one sampling
-- interval in the case of relatively sparse sampling, thus avoiding an
-- "event backlog" should sampling become more frequent at some later
-- point in time.

-- !!! Maybe it would better to give a frequency? But like this to make
-- !!! consitent with "repeatedly".
occasionally :: RandomGen g => g -> Time -> b -> SF a (Event b)
occasionally g t_avg x | t_avg > 0 = SF {sfTF = tf0}
                       | otherwise = usrErr "AFRP" "occasionally"
				            "Non-positive average interval."
    where
	-- Generally, if events occur with an average frequency of f, the
	-- probability of at least one event occurring in an interval of t
        -- is given by (1 - exp (-f*t)). The goal in the following is to
	-- decide whether at least one event occurred in the interval of size
	-- dt preceding the current sample point. For the first point,
	-- we can think of the preceding interval as being 0, implying
	-- no probability of an event occurring.

    tf0 _ = (occAux (randoms g :: [Time]), NoEvent)

    occAux [] = undefined
    occAux (r:rs) = SF' tf -- True
        where
        tf dt _ = let p = 1 - exp (-(dt/t_avg)) -- Probability for at least one event.
                  in (occAux rs, if r < p then Event x else NoEvent)
                  

-- Vim modeline
-- vim:set tabstop=8 expandtab:
