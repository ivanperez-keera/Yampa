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

module FRP.Yampa.Switches (
    -- Re-exported module, classes, and types

-- * Switching
-- ** Basic switchers
    switch,  dSwitch,	-- :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
    rSwitch, drSwitch,	-- :: SF a b -> SF (a,Event (SF a b)) b
    kSwitch, dkSwitch,	-- :: SF a b
			--    -> SF (a,b) (Event c)
			--    -> (SF a b -> c -> SF a b)
			--    -> SF a b

-- ** Parallel composition and switching
-- *** Parallel composition and switching over collections with broadcasting
    parB,		-- :: Functor col => col (SF a b) -> SF a (col b)
    pSwitchB,dpSwitchB, -- :: Functor col =>
			--        col (SF a b)
			--	  -> SF (a, col b) (Event c)
			--	  -> (col (SF a b) -> c -> SF a (col b))
			--	  -> SF a (col b)
    rpSwitchB,drpSwitchB,-- :: Functor col =>
			--        col (SF a b)
			--	  -> SF (a, Event (col (SF a b)->col (SF a b)))
			--	        (col b)

-- *** Parallel composition and switching over collections with general routing
    par,		-- Functor col =>
    			--     (forall sf . (a -> col sf -> col (b, sf)))
    			--     -> col (SF b c)
    			--     -> SF a (col c)
    pSwitch, dpSwitch,  -- pSwitch :: Functor col =>
			--     (forall sf . (a -> col sf -> col (b, sf)))
			--     -> col (SF b c)
			--     -> SF (a, col c) (Event d)
			--     -> (col (SF b c) -> d -> SF a (col c))
			--     -> SF a (col c)
    rpSwitch,drpSwitch, -- Functor col =>
			--    (forall sf . (a -> col sf -> col (b, sf)))
    			--    -> col (SF b c)
			--    -> SF (a, Event (col (SF b c) -> col (SF b c)))
			--	    (col c)
) where

import Control.Arrow
#if __GLASGOW_HASKELL__ >= 610
import qualified Control.Category (Category(..))
#else
#endif
import Control.Monad (unless)
import Data.IORef
import Data.Maybe (fromMaybe)
import System.Random (RandomGen(..), Random(..))


import FRP.Yampa.Core
import FRP.Yampa.Diagnostics
import FRP.Yampa.Miscellany (( # ), dup, swap)
import FRP.Yampa.Event
import FRP.Yampa.VectorSpace
import FRP.Yampa.Integration

------------------------------------------------------------------------------
-- Basic switchers
------------------------------------------------------------------------------

-- !!! Interesting case. It seems we need scoped type variables
-- !!! to be able to write down the local type signatures.
-- !!! On the other hand, the scoped type variables seem to
-- !!! prohibit the kind of unification that is needed for GADTs???
-- !!! Maybe this could be made to wok if it actually WAS known
-- !!! that scoped type variables indeed corresponds to universally
-- !!! quantified variables? Or if one were to keep track of those
-- !!! scoped type variables that actually do?
-- !!!
-- !!! Find a simpler case to experiment further. For now, elim.
-- !!! the free variable.

{-
-- Basic switch.
switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch (SF {sfTF = tf10} :: SF a (b, Event c)) (k :: c -> SF a b) = SF {sfTF = tf0}
    where
	tf0 a0 =
	    case tf10 a0 of
	    	(sf1, (b0, NoEvent))  -> (switchAux sf1, b0)
		(_,   (_,  Event c0)) -> sfTF (k c0) a0

        -- It would be nice to optimize further here. E.g. if it would be
        -- possible to observe the event source only.
        switchAux :: SF' a (b, Event c) -> SF' a b
        switchAux (SFId _)                 = switchAuxA1 id	-- New
	switchAux (SFConst _ (b, NoEvent)) = sfConst b
	switchAux (SFArr _ f1)             = switchAuxA1 f1
	switchAux sf1                      = SF' tf
	    where
		tf dt a =
		    case (sfTF' sf1) dt a of
			(sf1', (b, NoEvent)) -> (switchAux sf1', b)
			(_,    (_, Event c)) -> sfTF (k c) a

	-- Could be optimized a little bit further by having a case for
        -- identity, switchAuxI1

	-- Note: While switch behaves as a stateless arrow at this point, that
	-- could change after a switch. Hence, SF' overall.
        switchAuxA1 :: (a -> (b, Event c)) -> SF' a b
	switchAuxA1 f1 = sf
	    where
		sf     = SF' tf
		tf _ a =
		    case f1 a of
			(b, NoEvent) -> (sf, b)
			(_, Event c) -> sfTF (k c) a
-}

-- | Basic switch.
-- 
-- By default, the first signal function is applied.
--
-- Whenever the second value in the pair actually is an event,
-- the value carried by the event is used to obtain a new signal
-- function to be applied *at that time and at future times*.
-- 
-- Until that happens, the first value in the pair is produced
-- in the output signal.
--
-- Important note: at the time of switching, the second
-- signal function is applied immediately. If that second
-- SF can also switch at time zero, then a double (nested)
-- switch might take place. If the second SF refers to the
-- first one, the switch might take place infinitely many
-- times and never be resolved.
--
-- Remember: The continuation is evaluated strictly at the time
-- of switching!
switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
	tf0 a0 =
	    case tf10 a0 of
	    	(sf1, (b0, NoEvent))  -> (switchAux sf1 k, b0)
		(_,   (_,  Event c0)) -> sfTF (k c0) a0

        -- It would be nice to optimize further here. E.g. if it would be
        -- possible to observe the event source only.
        switchAux :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
	switchAux (SFArr _ (FDC (b, NoEvent))) _ = sfConst b
	switchAux (SFArr _ fd1)                k = switchAuxA1 (fdFun fd1) k
	switchAux sf1                          k = SF' tf
{-
	    if sfIsInv sf1 then
		switchInv sf1 k
	    else
		SF' tf False
-}
	    where
		tf dt a =
		    case (sfTF' sf1) dt a of
			(sf1', (b, NoEvent)) -> (switchAux sf1' k, b)
			(_,    (_, Event c)) -> sfTF (k c) a

{-
        -- Note: subordinate signal function being invariant does NOT
        -- imply that the overall signal function is.
        switchInv :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
	switchInv sf1 k = SF' tf False
	    where
		tf dt a =
		    case (sfTF' sf1) dt a of
			(sf1', (b, NoEvent)) -> (switchInv sf1' k, b)
			(_,    (_, Event c)) -> sfTF (k c) a
-}

	-- !!! Could be optimized a little bit further by having a case for
        -- !!! identity, switchAuxI1. But I'd expect identity is so unlikely
        -- !!! that there is no point.

	-- Note: While switch behaves as a stateless arrow at this point, that
	-- could change after a switch. Hence, SF' overall.
        switchAuxA1 :: (a -> (b, Event c)) -> (c -> SF a b) -> SF' a b
	switchAuxA1 f1 k = sf
	    where
		sf     = SF' tf -- False
		tf _ a =
		    case f1 a of
			(b, NoEvent) -> (sf, b)
			(_, Event c) -> sfTF (k c) a


-- | Switch with delayed observation.
-- 
-- By default, the first signal function is applied.
--
-- Whenever the second value in the pair actually is an event,
-- the value carried by the event is used to obtain a new signal
-- function to be applied *at future times*.
-- 
-- Until that happens, the first value in the pair is produced
-- in the output signal.
--
-- Important note: at the time of switching, the second
-- signal function is used immediately, but the current
-- input is fed by it (even though the actual output signal
-- value at time 0 is discarded). 
-- 
-- If that second SF can also switch at time zero, then a
-- double (nested) -- switch might take place. If the second SF refers to the
-- first one, the switch might take place infinitely many times and never be
-- resolved.
--
-- Remember: The continuation is evaluated strictly at the time
-- of switching!

-- Alternative name: "decoupled switch"?
-- (The SFId optimization is highly unlikley to be of much use, but it
-- does raise an interesting typing issue.)
dSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
dSwitch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
	tf0 a0 =
	    let (sf1, (b0, ec0)) = tf10 a0
            in (case ec0 of
                    NoEvent  -> dSwitchAux sf1 k
		    Event c0 -> fst (sfTF (k c0) a0),
                b0)

        -- It would be nice to optimize further here. E.g. if it would be
        -- possible to observe the event source only.
        dSwitchAux :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
	dSwitchAux (SFArr _ (FDC (b, NoEvent))) _ = sfConst b
	dSwitchAux (SFArr _ fd1)                k = dSwitchAuxA1 (fdFun fd1) k
	dSwitchAux sf1                          k = SF' tf
{-
	    if sfIsInv sf1 then
		dSwitchInv sf1 k
	    else
		SF' tf False
-}
	    where
		tf dt a =
		    let (sf1', (b, ec)) = (sfTF' sf1) dt a
                    in (case ec of
			    NoEvent -> dSwitchAux sf1' k
			    Event c -> fst (sfTF (k c) a),

			b)

{-
        -- Note: that the subordinate signal function is invariant does NOT
        -- imply that the overall signal function is.
        dSwitchInv :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
	dSwitchInv sf1 k = SF' tf False
	    where
		tf dt a =
		    let (sf1', (b, ec)) = (sfTF' sf1) dt a
                    in (case ec of
			    NoEvent -> dSwitchInv sf1' k
			    Event c -> fst (sfTF (k c) a),

			b)
-}

	-- !!! Could be optimized a little bit further by having a case for
        -- !!! identity, switchAuxI1

	-- Note: While dSwitch behaves as a stateless arrow at this point, that
	-- could change after a switch. Hence, SF' overall.
        dSwitchAuxA1 :: (a -> (b, Event c)) -> (c -> SF a b) -> SF' a b
	dSwitchAuxA1 f1 k = sf
	    where
		sf = SF' tf -- False
		tf _ a =
		    let (b, ec) = f1 a
                    in (case ec of
			    NoEvent -> sf
			    Event c -> fst (sfTF (k c) a),

			b)


-- | Recurring switch.
-- 
-- See <http://www.haskell.org/haskellwiki/Yampa#Switches> for more
-- information on how this switch works.

-- !!! Suboptimal. Overall, the constructor is invarying since rSwitch is
-- !!! being invoked recursively on a switch. In fact, we don't even care
-- !!! whether the subordinate signal function is invarying or not.
-- !!! We could make use of a signal function transformer sfInv to
-- !!! mark the constructor as invarying. Would that make sense?
-- !!! The price would be an extra loop with case analysis.
-- !!! The potential gain is fewer case analyses in superior loops.
rSwitch :: SF a b -> SF (a, Event (SF a b)) b
rSwitch sf = switch (first sf) ((noEventSnd >=-) . rSwitch)

{-
-- Old version. New is more efficient. Which one is clearer?
rSwitch :: SF a b -> SF (a, Event (SF a b)) b
rSwitch sf = switch (first sf) rSwitch'
    where
        rSwitch' sf = switch (sf *** notYet) rSwitch'
-}


-- | Recurring switch with delayed observation.
-- 
-- See <http://www.haskell.org/haskellwiki/Yampa#Switches> for more
-- information on how this switch works.
drSwitch :: SF a b -> SF (a, Event (SF a b)) b
drSwitch sf = dSwitch (first sf) ((noEventSnd >=-) . drSwitch)

{-
-- Old version. New is more efficient. Which one is clearer?
drSwitch :: SF a b -> SF (a, Event (SF a b)) b
drSwitch sf = dSwitch (first sf) drSwitch'
    where
        drSwitch' sf = dSwitch (sf *** notYet) drSwitch'
-}


-- | "Call-with-current-continuation" switch.
-- 
-- See <http://www.haskell.org/haskellwiki/Yampa#Switches> for more
-- information on how this switch works.

-- !!! Has not been optimized properly.
-- !!! Nor has opts been tested!
-- !!! Don't forget Inv opts!
kSwitch :: SF a b -> SF (a,b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
kSwitch sf10@(SF {sfTF = tf10}) (SF {sfTF = tfe0}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
	    let (sf1, b0) = tf10 a0
            in
	        case tfe0 (a0, b0) of
		    (sfe, NoEvent)  -> (kSwitchAux sf1 sfe, b0)
		    (_,   Event c0) -> sfTF (k sf10 c0) a0

-- Same problem as above: must pass k explicitly???
--        kSwitchAux (SFId _)      sfe                 = kSwitchAuxI1 sfe
        kSwitchAux (SFArr _ (FDC b)) sfe = kSwitchAuxC1 b sfe
        kSwitchAux (SFArr _ fd1)     sfe = kSwitchAuxA1 (fdFun fd1) sfe
        -- kSwitchAux (SFArrE _ f1)  sfe                 = kSwitchAuxA1 f1 sfe
        -- kSwitchAux (SFArrEE _ f1) sfe                 = kSwitchAuxA1 f1 sfe
        kSwitchAux sf1 (SFArr _ (FDC NoEvent)) = sf1
        kSwitchAux sf1 (SFArr _ fde) = kSwitchAuxAE sf1 (fdFun fde) 
        -- kSwitchAux sf1            (SFArrE _ fe)       = kSwitchAuxAE sf1 fe 
        -- kSwitchAux sf1            (SFArrEE _ fe)      = kSwitchAuxAE sf1 fe 
        kSwitchAux sf1            sfe                 = SF' tf -- False
	    where
		tf dt a =
		    let	(sf1', b) = (sfTF' sf1) dt a
		    in
		        case (sfTF' sfe) dt (a, b) of
			    (sfe', NoEvent) -> (kSwitchAux sf1' sfe', b)
			    (_,    Event c) -> sfTF (k (freeze sf1 dt) c) a

{-
-- !!! Untested optimization!
        kSwitchAuxI1 (SFConst _ NoEvent) = sfId
        kSwitchAuxI1 (SFArr _ fe)        = kSwitchAuxI1AE fe
        kSwitchAuxI1 sfe                 = SF' tf
	    where
		tf dt a =
		    case (sfTF' sfe) dt (a, a) of
			(sfe', NoEvent) -> (kSwitchAuxI1 sfe', a)
			(_,    Event c) -> sfTF (k identity c) a
-}

-- !!! Untested optimization!
        kSwitchAuxC1 b (SFArr _ (FDC NoEvent)) = sfConst b
        kSwitchAuxC1 b (SFArr _ fde)        = kSwitchAuxC1AE b (fdFun fde)
        -- kSwitchAuxC1 b (SFArrE _ fe)       = kSwitchAuxC1AE b fe
        -- kSwitchAuxC1 b (SFArrEE _ fe)      = kSwitchAuxC1AE b fe
        kSwitchAuxC1 b sfe                 = SF' tf -- False
	    where
		tf dt a =
		    case (sfTF' sfe) dt (a, b) of
			(sfe', NoEvent) -> (kSwitchAuxC1 b sfe', b)
			(_,    Event c) -> sfTF (k (constant b) c) a

-- !!! Untested optimization!
        kSwitchAuxA1 f1 (SFArr _ (FDC NoEvent)) = sfArrG f1
        kSwitchAuxA1 f1 (SFArr _ fde)        = kSwitchAuxA1AE f1 (fdFun fde)
        -- kSwitchAuxA1 f1 (SFArrE _ fe)       = kSwitchAuxA1AE f1 fe
        -- kSwitchAuxA1 f1 (SFArrEE _ fe)      = kSwitchAuxA1AE f1 fe
        kSwitchAuxA1 f1 sfe                 = SF' tf -- False
	    where
		tf dt a =
		    let	b = f1 a
		    in
		        case (sfTF' sfe) dt (a, b) of
			    (sfe', NoEvent) -> (kSwitchAuxA1 f1 sfe', b)
			    (_,    Event c) -> sfTF (k (arr f1) c) a

-- !!! Untested optimization!
--        kSwitchAuxAE (SFId _)      fe = kSwitchAuxI1AE fe
        kSwitchAuxAE (SFArr _ (FDC b))  fe = kSwitchAuxC1AE b fe
        kSwitchAuxAE (SFArr _ fd1)   fe = kSwitchAuxA1AE (fdFun fd1) fe
        -- kSwitchAuxAE (SFArrE _ f1)  fe = kSwitchAuxA1AE f1 fe
        -- kSwitchAuxAE (SFArrEE _ f1) fe = kSwitchAuxA1AE f1 fe
        kSwitchAuxAE sf1            fe = SF' tf -- False
	    where
		tf dt a =
		    let	(sf1', b) = (sfTF' sf1) dt a
		    in
		        case fe (a, b) of
			    NoEvent -> (kSwitchAuxAE sf1' fe, b)
			    Event c -> sfTF (k (freeze sf1 dt) c) a

{-
-- !!! Untested optimization!
        kSwitchAuxI1AE fe = SF' tf -- False
	    where
		tf dt a =
		    case fe (a, a) of
			NoEvent -> (kSwitchAuxI1AE fe, a)
			Event c -> sfTF (k identity c) a
-}

-- !!! Untested optimization!
        kSwitchAuxC1AE b fe = SF' tf -- False
	    where
		tf _ a =
		    case fe (a, b) of
			NoEvent -> (kSwitchAuxC1AE b fe, b)
			Event c -> sfTF (k (constant b) c) a

-- !!! Untested optimization!
        kSwitchAuxA1AE f1 fe = SF' tf -- False
	    where
		tf _ a =
		    let	b = f1 a
		    in
		        case fe (a, b) of
			    NoEvent -> (kSwitchAuxA1AE f1 fe, b)
			    Event c -> sfTF (k (arr f1) c) a


-- | 'kSwitch' with delayed observation.
-- 
-- See <http://www.haskell.org/haskellwiki/Yampa#Switches> for more
-- information on how this switch works.

-- !!! Has not been optimized properly. Should be like kSwitch.
dkSwitch :: SF a b -> SF (a,b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
dkSwitch sf10@(SF {sfTF = tf10}) (SF {sfTF = tfe0}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
	    let (sf1, b0) = tf10 a0
            in (case tfe0 (a0, b0) of
		    (sfe, NoEvent)  -> dkSwitchAux sf1 sfe
		    (_,   Event c0) -> fst (sfTF (k sf10 c0) a0),
                b0)

        dkSwitchAux sf1 (SFArr _ (FDC NoEvent)) = sf1
        dkSwitchAux sf1 sfe                     = SF' tf -- False
	    where
		tf dt a =
		    let	(sf1', b) = (sfTF' sf1) dt a
		    in (case (sfTF' sfe) dt (a, b) of
			    (sfe', NoEvent) -> dkSwitchAux sf1' sfe'
			    (_, Event c) -> fst (sfTF (k (freeze sf1 dt) c) a),
		        b)


------------------------------------------------------------------------------
-- Parallel composition and switching over collections with broadcasting
------------------------------------------------------------------------------

-- | Tuple a value up with every element of a collection of signal
-- functions.
broadcast :: Functor col => a -> col sf -> col (a, sf)
broadcast a sfs = fmap (\sf -> (a, sf)) sfs


-- !!! Hmm. We should really optimize here.
-- !!! Check for Arr in parallel!
-- !!! Check for Arr FDE in parallel!!!
-- !!! Check for EP in parallel!!!!!
-- !!! Cf &&&.
-- !!! But how??? All we know is that the collection is a functor ...
-- !!! Maybe that kind of generality does not make much sense for
-- !!! par and parB? (Although it is niceto be able to switch into a
-- !!! par or parB from within a pSwitch[B].)
-- !!! If we had a parBList, that could be defined in terms of &&&, surely?
-- !!! E.g.
-- !!! parBList []       = constant []
-- !!! parBList (sf:sfs) = sf &&& parBList sfs >>> arr (\(x,xs) -> x:xs)
-- !!!
-- !!! This ought to optimize quite well. E.g.
-- !!! parBList [arr1,arr2,arr3]
-- !!! = arr1 &&& parBList [arr2,arr3] >>> arrX
-- !!! = arr1 &&& (arr2 &&& parBList [arr3] >>> arrX) >>> arrX
-- !!! = arr1 &&& (arr2 &&& (arr3 &&& parBList [] >>> arrX) >>> arrX) >>> arrX
-- !!! = arr1 &&& (arr2 &&& (arr3C >>> arrX) >>> arrX) >>> arrX
-- !!! = arr1 &&& (arr2 &&& (arr3CcpX) >>> arrX) >>> arrX
-- !!! = arr1 &&& (arr23CcpX >>> arrX) >>> arrX
-- !!! = arr1 &&& (arr23CcpXcpX) >>> arrX
-- !!! = arr123CcpXcpXcpX

-- | Spatial parallel composition of a signal function collection.
-- Given a collection of signal functions, it returns a signal
-- function that 'broadcast's its input signal to every element
-- of the collection, to return a signal carrying a collection
-- of outputs. See 'par'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
parB :: Functor col => col (SF a b) -> SF a (col b)
parB = par broadcast

-- | Parallel switch (dynamic collection of signal functions spatially composed
-- in parallel). See 'pSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
pSwitchB :: Functor col =>
    col (SF a b) -> SF (a,col b) (Event c) -> (col (SF a b)->c-> SF a (col b))
    -> SF a (col b)
pSwitchB = pSwitch broadcast

-- | Delayed parallel switch with broadcasting (dynamic collection of
--   signal functions spatially composed in parallel). See 'dpSwitch'.
-- 
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
dpSwitchB :: Functor col =>
    col (SF a b) -> SF (a,col b) (Event c) -> (col (SF a b)->c->SF a (col b))
    -> SF a (col b)
dpSwitchB = dpSwitch broadcast

-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
rpSwitchB :: Functor col =>
    col (SF a b) -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)
rpSwitchB = rpSwitch broadcast

-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
drpSwitchB :: Functor col =>
    col (SF a b) -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)
drpSwitchB = drpSwitch broadcast


------------------------------------------------------------------------------
-- Parallel composition and switching over collections with general routing
------------------------------------------------------------------------------

-- | Spatial parallel composition of a signal function collection parameterized
-- on the routing function.
--
par :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf))) -- ^ Determines the input to each signal function
                                               --     in the collection. IMPORTANT! The routing function MUST
                                               --     preserve the structure of the signal function collection.

    -> col (SF b c)                            -- ^ Signal function collection.
    -> SF a (col c)
par rf sfs0 = SF {sfTF = tf0}
    where
	tf0 a0 =
	    let bsfs0 = rf a0 sfs0
		sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
		sfs   = fmap fst sfcs0
		cs0   = fmap snd sfcs0
	    in
		(parAux rf sfs, cs0)


-- Internal definition. Also used in parallel swithers.
parAux :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF' b c)
    -> SF' a (col c)
parAux rf sfs = SF' tf -- True
    where
	tf dt a = 
	    let bsfs  = rf a sfs
		sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
		sfs'  = fmap fst sfcs'
		cs    = fmap snd sfcs'
	    in
	        (parAux rf sfs', cs)


-- | Parallel switch parameterized on the routing function. This is the most
-- general switch from which all other (non-delayed) switches in principle
-- can be derived. The signal function collection is spatially composed in
-- parallel and run until the event signal function has an occurrence. Once
-- the switching event occurs, all signal function are "frozen" and their
-- continuations are passed to the continuation function, along with the
-- event value.
--

-- rf .........	Routing function: determines the input to each signal function
--		in the collection. IMPORTANT! The routing function has an
--		obligation to preserve the structure of the signal function
--		collection.
-- sfs0 .......	Signal function collection.
-- sfe0 .......	Signal function generating the switching event.
-- k .......... Continuation to be invoked once event occurs.
-- Returns the resulting signal function.
--
-- !!! Could be optimized on the event source being SFArr, SFArrE, SFArrEE
pSwitch :: Functor col
    => (forall sf . (a -> col sf -> col (b, sf))) -- ^ Routing function: determines the input to each signal function
                                                  --   in the collection. IMPORTANT! The routing function has an
                                                  --   obligation to preserve the structure of the signal function
                                                  --   collection.

    -> col (SF b c)                               -- ^ Signal function collection.
    -> SF (a, col c) (Event d)                    -- ^ Signal function generating the switching event.
    -> (col (SF b c) -> d -> SF a (col c))        -- ^ Continuation to be invoked once event occurs.
    -> SF a (col c)
pSwitch rf sfs0 sfe0 k = SF {sfTF = tf0}
    where
	tf0 a0 =
	    let bsfs0 = rf a0 sfs0
		sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
		sfs   = fmap fst sfcs0
		cs0   = fmap snd sfcs0
	    in
		case (sfTF sfe0) (a0, cs0) of
		    (sfe, NoEvent)  -> (pSwitchAux sfs sfe, cs0)
		    (_,   Event d0) -> sfTF (k sfs0 d0) a0

	pSwitchAux sfs (SFArr _ (FDC NoEvent)) = parAux rf sfs
	pSwitchAux sfs sfe = SF' tf -- False
	    where
		tf dt a =
		    let bsfs  = rf a sfs
			sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
			sfs'  = fmap fst sfcs'
			cs    = fmap snd sfcs'
		    in
			case (sfTF' sfe) dt (a, cs) of
			    (sfe', NoEvent) -> (pSwitchAux sfs' sfe', cs)
			    (_,    Event d) -> sfTF (k (freezeCol sfs dt) d) a


-- | Parallel switch with delayed observation parameterized on the routing
-- function.
--
-- The collection argument to the function invoked on the
-- switching event is of particular interest: it captures the
-- continuations of the signal functions running in the collection
-- maintained by 'dpSwitch' at the time of the switching event,
-- thus making it possible to preserve their state across a switch.
-- Since the continuations are plain, ordinary signal functions,
-- they can be resumed, discarded, stored, or combined with
-- other signal functions.

-- !!! Could be optimized on the event source being SFArr, SFArrE, SFArrEE.
--
dpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf))) -- ^ Routing function. Its purpose is
                                               --   to pair up each running signal function in the collection
                                               --   maintained by 'dpSwitch' with the input it is going to see
                                               --   at each point in time. All the routing function can do is specify
                                               --   how the input is distributed.
    -> col (SF b c)                            -- ^ Initial collection of signal functions.
    -> SF (a, col c) (Event d)                 -- ^ Signal function that observes the external
                                               --   input signal and the output signals from the collection in order
                                               --   to produce a switching event.
    -> (col (SF b c) -> d -> SF a (col c))     -- ^ The fourth argument is a function that is invoked when the
                                               --   switching event occurs, yielding a new signal function to switch
                                               --   into based on the collection of signal functions previously
                                               --   running and the value carried by the switching event. This
                                               --   allows the collection to be updated and then switched back
                                               --   in, typically by employing 'dpSwitch' again.
    -> SF a (col c)
dpSwitch rf sfs0 sfe0 k = SF {sfTF = tf0}
    where
	tf0 a0 =
	    let bsfs0 = rf a0 sfs0
		sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
		cs0   = fmap snd sfcs0
	    in
		(case (sfTF sfe0) (a0, cs0) of
		     (sfe, NoEvent)  -> dpSwitchAux (fmap fst sfcs0) sfe
		     (_,   Event d0) -> fst (sfTF (k sfs0 d0) a0),
	         cs0)

	dpSwitchAux sfs (SFArr _ (FDC NoEvent)) = parAux rf sfs
	dpSwitchAux sfs sfe = SF' tf -- False
	    where
		tf dt a =
		    let bsfs  = rf a sfs
			sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
			cs    = fmap snd sfcs'
		    in
			(case (sfTF' sfe) dt (a, cs) of
			     (sfe', NoEvent) -> dpSwitchAux (fmap fst sfcs')
							    sfe'
			     (_,    Event d) -> fst (sfTF (k (freezeCol sfs dt)
							     d)
							  a),
                         cs)


-- Recurring parallel switch parameterized on the routing function.
-- rf .........	Routing function: determines the input to each signal function
--		in the collection. IMPORTANT! The routing function has an
--		obligation to preserve the structure of the signal function
--		collection.
-- sfs ........	Initial signal function collection.
-- Returns the resulting signal function.

rpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c) -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)
rpSwitch rf sfs =
    pSwitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
    noEventSnd >=- rpSwitch rf (f sfs')


{-
rpSwitch rf sfs = pSwitch (rf . fst) sfs (arr (snd . fst)) k
    where
	k sfs f = rpSwitch' (f sfs)
	rpSwitch' sfs = pSwitch (rf . fst) sfs (NoEvent --> arr (snd . fst)) k
-}

-- Recurring parallel switch with delayed observation parameterized on the
-- routing function.
drpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c) -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)
drpSwitch rf sfs =
    dpSwitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
    noEventSnd >=- drpSwitch rf (f sfs')

{-
drpSwitch rf sfs = dpSwitch (rf . fst) sfs (arr (snd . fst)) k
    where
	k sfs f = drpSwitch' (f sfs)
	drpSwitch' sfs = dpSwitch (rf . fst) sfs (NoEvent-->arr (snd . fst)) k
-}

-- Vim modeline
-- vim:set tabstop=8 expandtab:
