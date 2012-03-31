{-# LANGUAGE GADTs, Rank2Types, CPP #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- New version using GADTs.
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

module FRP.Yampa (
-- Re-exported module, classes, and types
    module Control.Arrow,
    module FRP.Yampa.VectorSpace,
    RandomGen(..),
    Random(..),

-- Reverse function composition and arrow plumbing aids
    ( # ),		-- :: (a -> b) -> (b -> c) -> (a -> c),	infixl 9
    dup,		-- :: a -> (a,a)
    swap,		-- :: (a,b) -> (b,a)

-- Main types
    Time,	-- [s] Both for time w.r.t. some reference and intervals.
    SF,		-- Signal Function.
    Event(..),	-- Events; conceptually similar to Maybe (but abstract).

-- Temporray!
--    SF(..), sfTF',

-- Main instances
    -- SF is an instance of Arrow and ArrowLoop. Method instances:
    -- arr	:: (a -> b) -> SF a b
    -- (>>>)	:: SF a b -> SF b c -> SF a c
    -- (<<<)	:: SF b c -> SF a b -> SF a c
    -- first	:: SF a b -> SF (a,c) (b,c)
    -- second	:: SF a b -> SF (c,a) (c,b)
    -- (***)	:: SF a b -> SF a' b' -> SF (a,a') (b,b')
    -- (&&&)	:: SF a b -> SF a b' -> SF a (b,b')
    -- returnA	:: SF a a
    -- loop	:: SF (a,c) (b,c) -> SF a b

    -- Event is an instance of Functor, Eq, and Ord. Some method instances:
    -- fmap	:: (a -> b) -> Event a -> Event b
    -- (==)     :: Event a -> Event a -> Bool
    -- (<=)	:: Event a -> Event a -> Bool

-- For optimization
    arrPrim, arrEPrim,

-- Basic signal functions
    identity,		-- :: SF a a
    constant,		-- :: b -> SF a b
    localTime,		-- :: SF a Time
    time,               -- :: SF a Time,	Other name for localTime.

-- Initialization
    (-->),		-- :: b -> SF a b -> SF a b,		infixr 0
    (>--),		-- :: a -> SF a b -> SF a b,		infixr 0
    (-=>),              -- :: (b -> b) -> SF a b -> SF a b      infixr 0
    (>=-),              -- :: (a -> a) -> SF a b -> SF a b      infixr 0
    initially,		-- :: a -> SF a a

-- Simple, stateful signal processing
    sscan,		-- :: (b -> a -> b) -> b -> SF a b
    sscanPrim,		-- :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b

-- Basic event sources
    never, 		-- :: SF a (Event b)
    now,		-- :: b -> SF a (Event b)
    after,		-- :: Time -> b -> SF a (Event b)
    repeatedly,		-- :: Time -> b -> SF a (Event b)
    afterEach,		-- :: [(Time,b)] -> SF a (Event b)
    afterEachCat,       -- :: [(Time,b)] -> SF a (Event [b])
    delayEvent,		-- :: Time -> SF (Event a) (Event a)
    delayEventCat,	-- :: Time -> SF (Event a) (Event [a])
    edge,		-- :: SF Bool (Event ())
    iEdge,		-- :: Bool -> SF Bool (Event ())
    edgeTag,		-- :: a -> SF Bool (Event a)
    edgeJust,		-- :: SF (Maybe a) (Event a)
    edgeBy,		-- :: (a -> a -> Maybe b) -> a -> SF a (Event b)

-- Stateful event suppression
    notYet,		-- :: SF (Event a) (Event a)
    once,		-- :: SF (Event a) (Event a)
    takeEvents,		-- :: Int -> SF (Event a) (Event a)
    dropEvents,		-- :: Int -> SF (Event a) (Event a)

-- Basic switchers
    switch,  dSwitch,	-- :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
    rSwitch, drSwitch,	-- :: SF a b -> SF (a,Event (SF a b)) b
    kSwitch, dkSwitch,	-- :: SF a b
			--    -> SF (a,b) (Event c)
			--    -> (SF a b -> c -> SF a b)
			--    -> SF a b

-- Parallel composition and switching over collections with broadcasting
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

-- Parallel composition and switching over collections with general routing
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

-- Wave-form generation
    old_hold,		-- :: a -> SF (Event a) a
    hold,		-- :: a -> SF (Event a) a
    dHold,		-- :: a -> SF (Event a) a
    trackAndHold,	-- :: a -> SF (Maybe a) a

-- Accumulators
    old_accum,		-- :: a -> SF (Event (a -> a)) (Event a)
    old_accumBy,	-- :: (b -> a -> b) -> b -> SF (Event a) (Event b)
    old_accumFilter,	-- :: (c -> a -> (c, Maybe b)) -> c
    accum,		-- :: a -> SF (Event (a -> a)) (Event a)
    accumHold,		-- :: a -> SF (Event (a -> a)) a
    dAccumHold,		-- :: a -> SF (Event (a -> a)) a
    accumBy,		-- :: (b -> a -> b) -> b -> SF (Event a) (Event b)
    accumHoldBy,	-- :: (b -> a -> b) -> b -> SF (Event a) b
    dAccumHoldBy,	-- :: (b -> a -> b) -> b -> SF (Event a) b
    accumFilter,	-- :: (c -> a -> (c, Maybe b)) -> c
			--    -> SF (Event a) (Event b)

-- Delays
    old_pre, old_iPre,
    pre,		-- :: SF a a
    iPre,		-- :: a -> SF a a

-- Timed delays
    delay,		-- :: Time -> a -> SF a a

-- Integration and differentiation
    integral,		-- :: VectorSpace a s => SF a a

    derivative,		-- :: VectorSpace a s => SF a a		-- Crude!
    imIntegral,		-- :: VectorSpace a s => a -> SF a a

-- Loops with guaranteed well-defined feedback
    loopPre, 		-- :: c -> SF (a,c) (b,c) -> SF a b
    loopIntegral,	-- :: VectorSpace c s => SF (a,c) (b,c) -> SF a b

-- Pointwise functions on events
    noEvent,		-- :: Event a
    noEventFst,		-- :: (Event a, b) -> (Event c, b)
    noEventSnd,		-- :: (a, Event b) -> (a, Event c)
    event, 		-- :: a -> (b -> a) -> Event b -> a
    fromEvent,		-- :: Event a -> a
    isEvent,		-- :: Event a -> Bool
    isNoEvent,		-- :: Event a -> Bool
    tag, 		-- :: Event a -> b -> Event b,		infixl 8
    tagWith,            -- :: b -> Event a -> Event b,
    attach,		-- :: Event a -> b -> Event (a, b),	infixl 8
    lMerge, 		-- :: Event a -> Event a -> Event a,	infixl 6
    rMerge,		-- :: Event a -> Event a -> Event a,	infixl 6
    merge,		-- :: Event a -> Event a -> Event a,	infixl 6
    mergeBy,		-- :: (a -> a -> a) -> Event a -> Event a -> Event a
    mapMerge,           -- :: (a -> c) -> (b -> c) -> (a -> b -> c) 
                        --    -> Event a -> Event b -> Event c
    mergeEvents,        -- :: [Event a] -> Event a
    catEvents,		-- :: [Event a] -> Event [a]
    joinE,		-- :: Event a -> Event b -> Event (a,b),infixl 7
    splitE,		-- :: Event (a,b) -> (Event a, Event b)
    filterE,	 	-- :: (a -> Bool) -> Event a -> Event a
    mapFilterE,		-- :: (a -> Maybe b) -> Event a -> Event b
    gate,		-- :: Event a -> Bool -> Event a,	infixl 8

-- Noise (random signal) sources and stochastic event sources
    noise,		-- :: noise :: (RandomGen g, Random b) =>
			--        g -> SF a b
    noiseR,		-- :: noise :: (RandomGen g, Random b) =>
			--        (b,b) -> g -> SF a b
    occasionally,	-- :: RandomGen g => g -> Time -> b -> SF a (Event b)

-- Reactimation
    reactimate,		-- :: IO a
	      		--    -> (Bool -> IO (DTime, Maybe a))
	      		--    -> (Bool -> b -> IO Bool)
              		--    -> SF a b
	      		--    -> IO ()
    ReactHandle,
    reactInit,          --    IO a -- init
                        --    -> (ReactHandle a b -> Bool -> b -> IO Bool) -- actuate
                        --    -> SF a b
                        --    -> IO (ReactHandle a b)
-- process a single input sample:
    react,              --    ReactHandle a b
                        --    -> (DTime,Maybe a)
                        --    -> IO Bool

-- Embedding (tentative: will be revisited)
    DTime,		-- [s] Sampling interval, always > 0.
    embed,		-- :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
    embedSynch,		-- :: SF a b -> (a, [(DTime, Maybe a)]) -> SF Double b
    deltaEncode,	-- :: Eq a => DTime -> [a] -> (a, [(DTime, Maybe a)])
    deltaEncodeBy 	-- :: (a -> a -> Bool) -> DTime -> [a]
			--    -> (a, [(DTime, Maybe a)])

) where

import Control.Monad (unless)
import System.Random (RandomGen(..), Random(..))

#if __GLASGOW_HASKELL__ >= 610
import qualified Control.Category (Category(..))
#else
#endif

import Control.Arrow
import FRP.Yampa.Diagnostics
import FRP.Yampa.Miscellany (( # ), dup, swap)
import FRP.Yampa.Event
import FRP.Yampa.VectorSpace

import Data.IORef

infixr 0 -->, >--, -=>, >=-

------------------------------------------------------------------------------
-- Basic type definitions with associated utilities
------------------------------------------------------------------------------

-- The time type is really a bit boguous, since, as time passes, the minimal
-- interval between two consecutive floating-point-represented time points
-- increases. A better approach might be to pick a reasonable resolution
-- and represent time and time intervals by Integer (giving the number of
-- "ticks").
--
-- That might also improve the timing of time-based event sources.
-- One might actually pick the overall resolution in reactimate,
-- to be passed down, possibly in the form of a global parameter
-- record, to all signal functions on initialization. (I think only
-- switch would need to remember the record, since it is the only place
-- where signal functions get started. So it wouldn't cost all that much.

-- Time is used both for time intervals (duration), and time w.r.t. some
-- agreed reference point in time. Conceptually, Time = R, i.e. time can be 0
-- or even negative.
type Time = Double	-- [s]


-- DTime is the time type for lengths of sample intervals. Conceptually,
-- DTime = R+ = { x in R | x > 0 }. Don't assume Time and DTime have the
-- same representation.

type DTime = Double	-- [s]


-- Representation of signal function in initial state.
-- (Naming: "TF" stands for Transition Function.)

data SF a b = SF {sfTF :: a -> Transition a b}


-- Representation of signal function in "running" state.
--
-- Possibly better design for Inv.
--   Problem: tension between on the one hand making use of the
--   invariant property, and on the other keeping track of how something
--   has been constructed (SFCpAXA, in particular).
--   Idea: Add a boolean field to SFCpAXA and SF' that classifies
--   a signal function as being invarying.
--   A function sfIsInv computes to True for SFArr, SFAcc (and SFSScan,
--   possibly more), extracts the field in other cases.
--
--  Motivation for using a function (Event a -> b) in SFArrE
--  rather than (a -> Event b) or (a -> b) or even (Event a -> Event b).
--    The result type should be just "b" as opposed to "Event b" for
--    increased flexibility (e.g. matching "routing functions").
--    When the result type actually IS (Event b), and this fact is
--    exploitable, we'll be in a context where is it clear that
--    this is a fact, so we don't lose anything.
--    Since the idea is that the function is only going to be applied
--    when the there is an event, one could imagine the input type
--    just "a". But that's not the type of function we're given,
--    so it would have to be "massaged" a bit (precomposing with Event)
--    to fit. This will gain nothing, and potentially we will lose if
--    we actually need to recover the original function.
--    In fact, we sometimes really need to recover the original function
--    (e.g. currently in switch), and to do it correctly (also handling
--    NoEvent), we'd have to work quite hard introducing further
--    inefficiencies.
--  Summary: Make use of what we are given and only wrap things up later
--  when it is clear whatthe need is going to be, thus avoiding costly
--  "unwrapping".

-- GADTs needed in particular for SFEP, but also e.g. SFSScan
-- exploits them since there are more type vars than in the type con.
-- But one could use existentials for those.


data SF' a b where
    SFArr   :: !(DTime -> a -> Transition a b) -> !(FunDesc a b) -> SF' a b
    -- The b is intentionally unstrict as the initial output sometimes
    -- is undefined (e.g. when defining pre). In any case, it isn't
    -- necessarily used and should thus not be forced.
    SFSScan :: !(DTime -> a -> Transition a b)
               -> !(c -> a -> Maybe (c, b)) -> !c -> b 
               -> SF' a b
    SFEP   :: !(DTime -> Event a -> Transition (Event a) b)
              -> !(c -> a -> (c, b, b)) -> !c -> b
              -> SF' (Event a) b
    SFCpAXA :: !(DTime -> a -> Transition a d)
               -> !(FunDesc a b) -> !(SF' b c) -> !(FunDesc c d)
               -> SF' a d
    --  SFPair :: ...
    SF' :: !(DTime -> a -> Transition a b) -> SF' a b

-- A transition is a pair of the next state (in the form of a signal
-- function) and the output at the present time step.

type Transition a b = (SF' a b, b)


sfTF' :: SF' a b -> (DTime -> a -> Transition a b)
sfTF' (SFArr tf _)       = tf
sfTF' (SFSScan tf _ _ _) = tf
sfTF' (SFEP tf _ _ _)    = tf
sfTF' (SFCpAXA tf _ _ _) = tf
sfTF' (SF' tf)           = tf


-- !!! 2005-06-30
-- Unclear why, but the isInv mechanism seems to do more
-- harm than good.
-- Disable completely and see what happens.
{-
sfIsInv :: SF' a b -> Bool
-- sfIsInv _ = False
sfIsInv (SFArr _ _)           = True
-- sfIsInv (SFAcc _ _ _ _)       = True
sfIsInv (SFEP _ _ _ _)        = True
-- sfIsInv (SFSScan ...) = True
sfIsInv (SFCpAXA _ inv _ _ _) = inv
sfIsInv (SF' _ inv)           = inv
-}

-- "Smart" constructors. The corresponding "raw" constructors should not
-- be used directly for construction.

sfArr :: FunDesc a b -> SF' a b
sfArr FDI         = sfId
sfArr (FDC b)     = sfConst b
sfArr (FDE f fne) = sfArrE f fne
sfArr (FDG f)     = sfArrG f


sfId :: SF' a a
sfId = sf
    where
	sf = SFArr (\_ a -> (sf, a)) FDI


sfConst :: b -> SF' a b
sfConst b = sf
    where
	sf = SFArr (\_ _ -> (sf, b)) (FDC b)


sfNever :: SF' a (Event b)
sfNever = sfConst NoEvent


-- Assumption: fne = f NoEvent
sfArrE :: (Event a -> b) -> b -> SF' (Event a) b
sfArrE f fne = sf
    where
        sf  = SFArr (\_ ea -> (sf, case ea of NoEvent -> fne ; _ -> f ea))
                    (FDE f fne)

sfArrG :: (a -> b) -> SF' a b
sfArrG f = sf
    where
	sf = SFArr (\_ a -> (sf, f a)) (FDG f)


sfSScan :: (c -> a -> Maybe (c, b)) -> c -> b -> SF' a b
sfSScan f c b = sf 
    where
        sf = SFSScan tf f c b
	tf _ a = case f c a of
		     Nothing       -> (sf, b)
		     Just (c', b') -> (sfSScan f c' b', b')

sscanPrim :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b
sscanPrim f c_init b_init = SF {sfTF = tf0}
    where
        tf0 a0 = case f c_init a0 of
                     Nothing       -> (sfSScan f c_init b_init, b_init)
	             Just (c', b') -> (sfSScan f c' b', b')


-- The event-processing function *could* accept the present NoEvent
-- output as an extra state argument. That would facilitate composition
-- of event-processing functions somewhat, but would presumably incur an
-- extra cost for the more common and simple case of non-composed event
-- processors.
-- 
sfEP :: (c -> a -> (c, b, b)) -> c -> b -> SF' (Event a) b
sfEP f c bne = sf
    where
        sf = SFEP (\_ ea -> case ea of
                                 NoEvent -> (sf, bne)
                                 Event a -> let
                                                (c', b, bne') = f c a
                                            in
                                                (sfEP f c' bne', b))
                  f
                  c
                  bne


-- epPrim is used to define hold, accum, and other event-processing
-- functions.
epPrim :: (c -> a -> (c, b, b)) -> c -> b -> SF (Event a) b
epPrim f c bne = SF {sfTF = tf0}
    where
        tf0 NoEvent   = (sfEP f c bne, bne)
        tf0 (Event a) = let
                            (c', b, bne') = f c a
                        in
                            (sfEP f c' bne', b)


{-
-- !!! Maybe something like this?
-- !!! But one problem is that the invarying marking would be lost
-- !!! if the signal function is taken apart and re-constructed from
-- !!! the function description and subordinate signal function in
-- !!! cases like SFCpAXA.
sfMkInv :: SF a b -> SF a b
sfMkInv sf = SF {sfTF = ...}

    sfMkInvAux :: SF' a b -> SF' a b
    sfMkInvAux sf@(SFArr _ _) = sf
    -- sfMkInvAux sf@(SFAcc _ _ _ _) = sf
    sfMkInvAux sf@(SFEP _ _ _ _) = sf
    sfMkInvAux sf@(SFCpAXA tf inv fd1 sf2 fd3)
	| inv       = sf
	| otherwise = SFCpAXA tf' True fd1 sf2 fd3
        where
            tf' = \dt a -> let (sf', b) = tf dt a in (sfMkInvAux sf', b)
    sfMkInvAux sf@(SF' tf inv)
        | inv       = sf
        | otherwise = SF' tf' True
            tf' = 

-}

-- Motivation for event-processing function type
-- (alternative would be function of type a->b plus ensuring that it
-- only ever gets invoked on events):
-- * Now we need to be consistent with other kinds of arrows.
-- * We still want to be able to get hold of the original function.
-- 2005-02-30: OK, for FDE, invarant is that the field of type b =
-- f NoEvent.

data FunDesc a b where
    FDI :: FunDesc a a					-- Identity function
    FDC :: b -> FunDesc a b				-- Constant function
    FDE :: (Event a -> b) -> b -> FunDesc (Event a) b	-- Event-processing fun
    FDG :: (a -> b) -> FunDesc a b			-- General function

fdFun :: FunDesc a b -> (a -> b)
fdFun FDI       = id
fdFun (FDC b)   = const b
fdFun (FDE f _) = f
fdFun (FDG f)   = f

fdComp :: FunDesc a b -> FunDesc b c -> FunDesc a c
fdComp FDI           fd2     = fd2
fdComp fd1           FDI     = fd1
fdComp (FDC b)       fd2     = FDC ((fdFun fd2) b)
fdComp _             (FDC c) = FDC c
-- Hardly worth the effort?
-- 2005-03-30: No, not only not worth the effort as the only thing saved
-- would be an application of f2. Also wrong since current invariant does
-- not imply that f1ne = NoEvent. Moreover, we cannot really adopt that
-- invariant as it is not totally impossible for a user to create a function
-- that breaks it.
-- fdComp (FDE f1 f1ne) (FDE f2 f2ne) =
--    FDE (f2 . f1) (vfyNoEvent (f1 NoEvent) f2ne)
fdComp (FDE f1 f1ne) fd2 = FDE (f2 . f1) (f2 f1ne)
    where
        f2 = fdFun fd2
fdComp (FDG f1) (FDE f2 f2ne) = FDG f
    where
        f a = case f1 a of
                  NoEvent -> f2ne
                  f1a     -> f2 f1a
fdComp (FDG f1) fd2 = FDG (fdFun fd2 . f1)


fdPar :: FunDesc a b -> FunDesc c d -> FunDesc (a,c) (b,d)
fdPar FDI     FDI     = FDI
fdPar FDI     (FDC d) = FDG (\(~(a, _)) -> (a, d))
fdPar FDI     fd2     = FDG (\(~(a, c)) -> (a, (fdFun fd2) c))
fdPar (FDC b) FDI     = FDG (\(~(_, c)) -> (b, c))
fdPar (FDC b) (FDC d) = FDC (b, d)
fdPar (FDC b) fd2     = FDG (\(~(_, c)) -> (b, (fdFun fd2) c))
fdPar fd1     fd2     = FDG (\(~(a, c)) -> ((fdFun fd1) a, (fdFun fd2) c))


fdFanOut :: FunDesc a b -> FunDesc a c -> FunDesc a (b,c)
fdFanOut FDI     FDI     = FDG dup
fdFanOut FDI     (FDC c) = FDG (\a -> (a, c))
fdFanOut FDI     fd2     = FDG (\a -> (a, (fdFun fd2) a))
fdFanOut (FDC b) FDI     = FDG (\a -> (b, a))
fdFanOut (FDC b) (FDC c) = FDC (b, c)
fdFanOut (FDC b) fd2     = FDG (\a -> (b, (fdFun fd2) a))
fdFanOut (FDE f1 f1ne) (FDE f2 f2ne) = FDE f1f2 f1f2ne
    where
       f1f2 NoEvent      = f1f2ne
       f1f2 ea@(Event _) = (f1 ea, f2 ea)

       f1f2ne = (f1ne, f2ne)
fdFanOut fd1 fd2 =
    FDG (\a -> ((fdFun fd1) a, (fdFun fd2) a))


-- Verifies that the first argument is NoEvent. Returns the value of the
-- second argument that is the case. Raises an error otherwise.
-- Used to check that functions on events do not map NoEvent to Event
-- wherever that assumption is exploited.
vfyNoEv :: Event a -> b -> b
vfyNoEv NoEvent b = b
vfyNoEv _       _  = usrErr "AFRP" "vfyNoEv" "Assertion failed: Functions on events must not map NoEvent to Event."


-- Freezes a "running" signal function, i.e., turns it into a continuation in
-- the form of a plain signal function.
freeze :: SF' a b -> DTime -> SF a b
freeze sf dt = SF {sfTF = (sfTF' sf) dt}


freezeCol :: Functor col => col (SF' a b) -> DTime -> col (SF a b)
freezeCol sfs dt = fmap (flip freeze dt) sfs


------------------------------------------------------------------------------
-- Arrow instance and implementation
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 610
instance Control.Category.Category SF where
     (.) = flip compPrim 
     id = SF $ \x -> (sfId,x)
#else
#endif

instance Arrow SF where
    arr    = arrPrim
    first  = firstPrim
    second = secondPrim
    (***)  = parSplitPrim
    (&&&)  = parFanOutPrim
#if __GLASGOW_HASKELL__ >= 610
#else
    (>>>)  = compPrim
#endif


-- Lifting.
{-# NOINLINE arrPrim #-}
arrPrim :: (a -> b) -> SF a b
arrPrim f = SF {sfTF = \a -> (sfArrG f, f a)}


{-# RULES "arrPrim/arrEPrim" arrPrim = arrEPrim #-}

arrEPrim :: (Event a -> b) -> SF (Event a) b
arrEPrim f = SF {sfTF = \a -> (sfArrE f (f NoEvent), f a)}


-- Composition.
-- The definition exploits the following identities:
--     sf         >>> identity   = sf				-- New
--     identity   >>> sf         = sf				-- New
--     sf         >>> constant c = constant c
--     constant c >>> arr f      = constant (f c)
--     arr f      >>> arr g      = arr (g . f)
--
-- !!! Notes/Questions:
-- !!! How do we know that the optimizations terminate?
-- !!! Probably by some kind of size argument on the SF tree.
-- !!! E.g. (Hopefully) all compPrim optimizations are such that
-- !!! the number of compose nodes decrease.
-- !!! Should verify this!
--
-- !!! There is a tension between using SFInv to signal to superior
-- !!! signal functions that the subordinate signal function will not
-- !!! change form, and using SFCpAXA to allow fusion in the context
-- !!! of some suitable superior signal function.
compPrim :: SF a b -> SF b c -> SF a c
compPrim (SF {sfTF = tf10}) (SF {sfTF = tf20}) = SF {sfTF = tf0}
    where
	tf0 a0 = (cpXX sf1 sf2, c0)
	    where
		(sf1, b0) = tf10 a0
		(sf2, c0) = tf20 b0

-- The following defs are not local to compPrim because cpAXA needs to be
-- called from parSplitPrim.
-- Naming convention: cp<X><Y> where  <X> and <Y> is one of:
-- X - arbitrary signal function
-- A - arbitrary pure arrow
-- C - constant arrow
-- E - event-processing arrow
-- G - arrow known not to be identity, constant (C) or
--     event-processing (E).

cpXX :: SF' a b -> SF' b c -> SF' a c
cpXX (SFArr _ fd1)       sf2               = cpAX fd1 sf2
cpXX sf1                 (SFArr _ fd2)     = cpXA sf1 fd2
{-
-- !!! 2005-07-07: Too strict.
-- !!! But the question is if it is worth to define pre in terms of sscan ...
-- !!! It is slower than the simplest possible pre, and the kind of coding
-- !!! required to ensure that the laziness props of the second SF are
-- !!! preserved might just slow things down further ...
cpXX (SFSScan _ f1 s1 b) (SFSScan _ f2 s2 c) =
    sfSScan f (s1, b, s2, c) c
    where
        f (s1, b, s2, c) a =
            case f1 s1 a of
                Nothing ->
                    case f2 s2 b of
                        Nothing        -> Nothing
                        Just (s2', c') -> Just ((s1, b, s2', c'), c')
                Just (s1', b') ->
                    case f2 s2 b' of
                        Nothing        -> Just ((s1', b', s2, c), c)
                        Just (s2', c') -> Just ((s1', b', s2', c'), c')
-}
-- !!! 2005-07-07: Indeed, this is a bit slower than the code above (14%).
-- !!! But both are better than not composing (35% faster and 26% faster)!
cpXX (SFSScan _ f1 s1 b) (SFSScan _ f2 s2 c) =
    sfSScan f (s1, b, s2, c) c
    where
        f (s1, b, s2, c) a =
            let
                (u, s1',  b') = case f1 s1 a of
                                    Nothing       -> (True, s1, b)
                                    Just (s1',b') -> (False,  s1', b')
            in
                case f2 s2 b' of
                    Nothing | u         -> Nothing
                            | otherwise -> Just ((s1', b', s2, c), c)
                    Just (s2', c') -> Just ((s1', b', s2', c'), c')
cpXX (SFSScan _ f1 s1 eb) (SFEP _ f2 s2 cne) =
    sfSScan f (s1, eb, s2, cne) cne
    where
        f (s1, eb, s2, cne) a =
            case f1 s1 a of
                Nothing ->
                    case eb of
                        NoEvent -> Nothing
                        Event b ->
                            let (s2', c, cne') = f2 s2 b
                            in
                                Just ((s1, eb, s2', cne'), c)
                Just (s1', eb') ->
                    case eb' of
                        NoEvent -> Just ((s1', eb', s2, cne), cne)
                        Event b ->
                            let (s2', c, cne') = f2 s2 b
                            in
                                Just ((s1', eb', s2', cne'), c)
-- !!! 2005-07-09: This seems to yield only a VERY marginal speedup
-- !!! without seq. With seq, substantial speedup!
cpXX (SFEP _ f1 s1 bne) (SFSScan _ f2 s2 c) =
    sfSScan f (s1, bne, s2, c) c
    where
        f (s1, bne, s2, c) ea =
            let (u, s1', b', bne') = case ea of
                                         NoEvent -> (True, s1, bne, bne)
                                         Event a ->
                                             let (s1', b, bne') = f1 s1 a
                                             in
                                                  (False, s1', b, bne')
            in
                case f2 s2 b' of
                    Nothing | u         -> Nothing
                            | otherwise -> Just (seq s1' (s1', bne', s2, c), c)
                    Just (s2', c') -> Just (seq s1' (s1', bne', s2', c'), c')
-- The function "f" is invoked whenever an event is to be processed. It then
-- computes the output, the new state, and the new NoEvent output.
-- However, when sequencing event processors, the ones in the latter
-- part of the chain may not get invoked since previous ones may
-- decide not to "fire". But a "new" NoEvent output still has to be
-- produced, i.e. the old one retained. Since it cannot be computed by
-- invoking the last event-processing function in the chain, it has to
-- be remembered. Since the composite event-processing function remains
-- constant/unchanged, the NoEvent output has to be part of the state.
-- An alternarive would be to make the event-processing function take an
-- extra argument. But that is likely to make the simple case more
-- expensive. See note at sfEP.
cpXX (SFEP _ f1 s1 bne) (SFEP _ f2 s2 cne) =
    sfEP f (s1, s2, cne) (vfyNoEv bne cne)
    where
	f (s1, s2, cne) a =
	    case f1 s1 a of
		(s1', NoEvent, NoEvent) -> ((s1', s2, cne), cne, cne)
		(s1', Event b, NoEvent) ->
		    let (s2', c, cne') = f2 s2 b in ((s1', s2', cne'), c, cne')
                _ -> usrErr "AFRP" "cpXX" "Assertion failed: Functions on events must not map NoEvent to Event."
-- !!! 2005-06-28: Why isn't SFCpAXA (FDC ...) checked for?
-- !!! No invariant rules that out, and it would allow to drop the
-- !!! event processor ... Does that happen elsewhere?
cpXX sf1@(SFEP _ _ _ _) (SFCpAXA _ (FDE f21 f21ne) sf22 fd23) =
    cpXX (cpXE sf1 f21 f21ne) (cpXA sf22 fd23)
-- f21 will (hopefully) be invoked less frequently if merged with the
-- event processor.
cpXX sf1@(SFEP _ _ _ _) (SFCpAXA _ (FDG f21) sf22 fd23) =
    cpXX (cpXG sf1 f21) (cpXA sf22 fd23)
-- Only functions whose domain is known to be Event can be merged
-- from the left with event processors.
cpXX (SFCpAXA _ fd11 sf12 (FDE f13 f13ne)) sf2@(SFEP _ _ _ _) =
    cpXX (cpAX fd11 sf12) (cpEX f13 f13ne sf2) 
-- !!! Other cases to look out for:
-- !!! any sf >>> SFCpAXA = SFCpAXA if first arr is const.
-- !!! But the following will presumably not work due to type restrictions.
-- !!! Need to reconstruct sf2 I think.
-- cpXX sf1 sf2@(SFCpAXA _ _ (FDC b) sf22 fd23) = sf2
cpXX (SFCpAXA _ fd11 sf12 fd13) (SFCpAXA _ fd21 sf22 fd23) =
    -- Termination: The first argument to cpXX is no larger than
    -- the current first argument, and the second is smaller.
    cpAXA fd11 (cpXX (cpXA sf12 (fdComp fd13 fd21)) sf22) fd23
-- !!! 2005-06-27: The if below accounts for a significant slowdown.
-- !!! One would really like a cheme where opts only take place
-- !!! after a structural change ... 
-- cpXX sf1 sf2 = cpXXInv sf1 sf2
-- cpXX sf1 sf2 = cpXXAux sf1 sf2
cpXX sf1 sf2 = SF' tf --  False
    -- if sfIsInv sf1 && sfIsInv sf2 then cpXXInv sf1 sf2 else SF' tf False
    where
        tf dt a = (cpXX sf1' sf2', c)
	    where
	        (sf1', b) = (sfTF' sf1) dt a
		(sf2', c) = (sfTF' sf2) dt b


{-
cpXXAux sf1@(SF' _ _) sf2@(SF' _ _) = SF' tf False
    where
        tf dt a = (cpXXAux sf1' sf2', c)
	    where
	        (sf1', b) = (sfTF' sf1) dt a
		(sf2', c) = (sfTF' sf2) dt b
cpXXAux sf1 sf2 = SF' tf False
    where
        tf dt a = (cpXXAux sf1' sf2', c)
	    where
	        (sf1', b) = (sfTF' sf1) dt a
		(sf2', c) = (sfTF' sf2) dt b
-}

{-
cpXXAux sf1 sf2 | unsimplifiable sf1 sf2 = SF' tf False
                | otherwise = cpXX sf1 sf2
    where
        tf dt a = (cpXXAux sf1' sf2', c)
	    where
	        (sf1', b) = (sfTF' sf1) dt a
		(sf2', c) = (sfTF' sf2) dt b

        unsimplifiable sf1@(SF' _ _) sf2@(SF' _ _) = True
        unsimplifiable sf1           sf2           = True
-}
                     
{-
-- wrong ...
cpXXAux sf1@(SF' _ False)           sf2                         = SF' tf False
cpXXAux sf1@(SFCpAXA _ False _ _ _) sf2                         = SF' tf False
cpXXAux sf1                         sf2@(SF' _ False)           = SF' tf False
cpXXAux sf1                         sf2@(SFCpAXA _ False _ _ _) = SF' tf False
cpXXAux sf1 sf2 =
    if sfIsInv sf1 && sfIsInv sf2 then cpXXInv sf1 sf2 else SF' tf False
    where
        tf dt a = (cpXXAux sf1' sf2', c)
	    where
	        (sf1', b) = (sfTF' sf1) dt a
		(sf2', c) = (sfTF' sf2) dt b
-}

{-
cpXXInv sf1 sf2 = SF' tf True
    where
        tf dt a = sf1 `seq` sf2 `seq` (cpXXInv sf1' sf2', c)
	    where
	        (sf1', b) = (sfTF' sf1) dt a
		(sf2', c) = (sfTF' sf2) dt b
-}

-- !!! No. We need local defs. Keep fd1 and fd2. Extract f1 and f2
-- !!! once and fo all. Get rid of FDI and FDC at the top level.
-- !!! First local def. analyse sf2. SFArr, SFAcc etc. tf in
-- !!! recursive case just make use of f1 and f3.
-- !!! if sf2 is SFInv, that's delegated to a second local
-- !!! recursive def. that does not analyse sf2.

cpAXA :: FunDesc a b -> SF' b c -> FunDesc c d -> SF' a d
-- Termination: cpAX/cpXA, via cpCX, cpEX etc. only call cpAXA if sf2
-- is SFCpAXA, and then on the embedded sf and hence on a smaller arg.
cpAXA FDI     sf2 fd3     = cpXA sf2 fd3
cpAXA fd1     sf2 FDI     = cpAX fd1 sf2
cpAXA (FDC b) sf2 fd3     = cpCXA b sf2 fd3
cpAXA _       _   (FDC d) = sfConst d        
cpAXA fd1     sf2 fd3     = 
    cpAXAAux fd1 (fdFun fd1) fd3 (fdFun fd3) sf2
    where
        -- Really: cpAXAAux :: SF' b c -> SF' a d
	-- Note: Event cases are not optimized (EXA etc.)
        cpAXAAux :: FunDesc a b -> (a -> b) -> FunDesc c d -> (c -> d)
                    -> SF' b c -> SF' a d
        cpAXAAux fd1 _ fd3 _ (SFArr _ fd2) =
            sfArr (fdComp (fdComp fd1 fd2) fd3)
        cpAXAAux fd1 _ fd3 _ sf2@(SFSScan _ _ _ _) =
            cpAX fd1 (cpXA sf2 fd3)
        cpAXAAux fd1 _ fd3 _ sf2@(SFEP _ _ _ _) =
            cpAX fd1 (cpXA sf2 fd3)
        cpAXAAux fd1 _ fd3 _ (SFCpAXA _ fd21 sf22 fd23) =
            cpAXA (fdComp fd1 fd21) sf22 (fdComp fd23 fd3)
        cpAXAAux fd1 f1 fd3 f3 sf2 = SFCpAXA tf fd1 sf2 fd3
{-
            if sfIsInv sf2 then
		cpAXAInv fd1 f1 fd3 f3 sf2
	    else
		SFCpAXA tf False fd1 sf2 fd3
-}
	    where
		tf dt a = (cpAXAAux fd1 f1 fd3 f3 sf2', f3 c)
		    where
			(sf2', c) = (sfTF' sf2) dt (f1 a)

{-
	cpAXAInv fd1 f1 fd3 f3 sf2 = SFCpAXA tf True fd1 sf2 fd3
	    where
		tf dt a = sf2 `seq` (cpAXAInv fd1 f1 fd3 f3 sf2', f3 c)
		    where
			(sf2', c) = (sfTF' sf2) dt (f1 a)
-}

cpAX :: FunDesc a b -> SF' b c -> SF' a c
cpAX FDI           sf2 = sf2
cpAX (FDC b)       sf2 = cpCX b sf2
cpAX (FDE f1 f1ne) sf2 = cpEX f1 f1ne sf2
cpAX (FDG f1)      sf2 = cpGX f1 sf2

cpXA :: SF' a b -> FunDesc b c -> SF' a c
cpXA sf1 FDI           = sf1
cpXA _   (FDC c)       = sfConst c
cpXA sf1 (FDE f2 f2ne) = cpXE sf1 f2 f2ne
cpXA sf1 (FDG f2)      = cpXG sf1 f2

-- Don't forget that the remaining signal function, if it is
-- SF', later could turn into something else, like SFId.
cpCX :: b -> SF' b c -> SF' a c
cpCX b (SFArr _ fd2) = sfConst ((fdFun fd2) b)
-- 2005-07-01:  If we were serious about the semantics of sscan being required
-- to be independent of the sampling interval, I guess one could argue for a
-- fixed-point computation here ... Or maybe not.
-- cpCX b (SFSScan _ _ _ _) = sfConst <fixed point comp>
cpCX b (SFSScan _ f s c) = sfSScan (\s _ -> f s b) s c
cpCX b (SFEP _ _ _ cne) = sfConst (vfyNoEv b cne)
cpCX b (SFCpAXA _ fd21 sf22 fd23) =
    cpCXA ((fdFun fd21) b) sf22 fd23
cpCX b sf2 = SFCpAXA tf (FDC b) sf2 FDI
{-
    if sfIsInv sf2 then
        cpCXInv b sf2
    else
	SFCpAXA tf False (FDC b) sf2 FDI
-}
    where
	tf dt _ = (cpCX b sf2', c)
	    where
		(sf2', c) = (sfTF' sf2) dt b


{-
cpCXInv b sf2 = SFCpAXA tf True (FDC b) sf2 FDI
    where
	tf dt _ = sf2 `seq` (cpCXInv b sf2', c)
	    where
		(sf2', c) = (sfTF' sf2) dt b
-}


cpCXA :: b -> SF' b c -> FunDesc c d -> SF' a d
cpCXA b sf2 FDI     = cpCX b sf2
cpCXA _ _   (FDC c) = sfConst c
cpCXA b sf2 fd3     = cpCXAAux (FDC b) b fd3 (fdFun fd3) sf2
    where
        -- fd1 = FDC b
        -- f3  = fdFun fd3

	-- Really: SF' b c -> SF' a d
        cpCXAAux :: FunDesc a b -> b -> FunDesc c d -> (c -> d)
                    -> SF' b c -> SF' a d
        cpCXAAux _ b _ f3 (SFArr _ fd2)     = sfConst (f3 ((fdFun fd2) b))
        cpCXAAux _ b _ f3 (SFSScan _ f s c) = sfSScan f' s (f3 c)
            where
	        f' s _ = case f s b of
                             Nothing -> Nothing
                             Just (s', c') -> Just (s', f3 c') 
        cpCXAAux _ b _   f3 (SFEP _ _ _ cne) = sfConst (f3 (vfyNoEv b cne))
        cpCXAAux _ b fd3 _  (SFCpAXA _ fd21 sf22 fd23) =
	    cpCXA ((fdFun fd21) b) sf22 (fdComp fd23 fd3)
	cpCXAAux fd1 b fd3 f3 sf2 = SFCpAXA tf fd1 sf2 fd3
{-
	    if sfIsInv sf2 then
		cpCXAInv fd1 b fd3 f3 sf2
            else
	        SFCpAXA tf False fd1 sf2 fd3
-}
	    where
		tf dt _ = (cpCXAAux fd1 b fd3 f3 sf2', f3 c)
		    where
			(sf2', c) = (sfTF' sf2) dt b

{-
        -- For some reason, seq on sf2' in tf is faster than making
        -- cpCXAInv strict in sf2 by seq-ing on the top level (which would
	-- be similar to pattern matching on sf2).
	cpCXAInv fd1 b fd3 f3 sf2 = SFCpAXA tf True fd1 sf2 fd3
	    where
		tf dt _ = sf2 `seq` (cpCXAInv fd1 b fd3 f3 sf2', f3 c)
		    where
			(sf2', c) = (sfTF' sf2) dt b
-}


cpGX :: (a -> b) -> SF' b c -> SF' a c
cpGX f1 sf2 = cpGXAux (FDG f1) f1 sf2
    where
	cpGXAux :: FunDesc a b -> (a -> b) -> SF' b c -> SF' a c
	cpGXAux fd1 _ (SFArr _ fd2) = sfArr (fdComp fd1 fd2)
        -- We actually do know that (fdComp (FDG f1) fd21) is going to
	-- result in an FDG. So we *could* call a cpGXA here. But the
	-- price is "inlining" of part of fdComp.
        cpGXAux _ f1 (SFSScan _ f s c) = sfSScan (\s a -> f s (f1 a)) s c
        -- We really shouldn't see an EP here, as that would mean
        -- an arrow INTRODUCING events ...
	cpGXAux fd1 _ (SFCpAXA _ fd21 sf22 fd23) =
	    cpAXA (fdComp fd1 fd21) sf22 fd23
	cpGXAux fd1 f1 sf2 = SFCpAXA tf fd1 sf2 FDI
{-
	    if sfIsInv sf2 then
	        cpGXInv fd1 f1 sf2
	    else
	        SFCpAXA tf False fd1 sf2 FDI
-}
	    where
		tf dt a = (cpGXAux fd1 f1 sf2', c)
		    where
			(sf2', c) = (sfTF' sf2) dt (f1 a)

{-
	cpGXInv fd1 f1 sf2 = SFCpAXA tf True fd1 sf2 FDI
	    where
		tf dt a = sf2 `seq` (cpGXInv fd1 f1 sf2', c)
		    where
			(sf2', c) = (sfTF' sf2) dt (f1 a)
-}


cpXG :: SF' a b -> (b -> c) -> SF' a c
cpXG sf1 f2 = cpXGAux (FDG f2) f2 sf1
    where
	-- Really: cpXGAux :: SF' a b -> SF' a c
	cpXGAux :: FunDesc b c -> (b -> c) -> SF' a b -> SF' a c
	cpXGAux fd2 _ (SFArr _ fd1) = sfArr (fdComp fd1 fd2)
        cpXGAux _ f2 (SFSScan _ f s b) = sfSScan f' s (f2 b)
            where
	        f' s a = case f s a of
                             Nothing -> Nothing
                             Just (s', b') -> Just (s', f2 b') 
        cpXGAux _ f2 (SFEP _ f1 s bne) = sfEP f s (f2 bne)
            where
                f s a = let (s', b, bne') = f1 s a in (s', f2 b, f2 bne')
	cpXGAux fd2 _ (SFCpAXA _ fd11 sf12 fd22) =
            cpAXA fd11 sf12 (fdComp fd22 fd2)
	cpXGAux fd2 f2 sf1 = SFCpAXA tf FDI sf1 fd2
{-
	    if sfIsInv sf1 then
		cpXGInv fd2 f2 sf1
	    else
		SFCpAXA tf False FDI sf1 fd2
-}
	    where
		tf dt a = (cpXGAux fd2 f2 sf1', f2 b)
		    where
			(sf1', b) = (sfTF' sf1) dt a

{-
	cpXGInv fd2 f2 sf1 = SFCpAXA tf True FDI sf1 fd2
	    where
		tf dt a = (cpXGInv fd2 f2 sf1', f2 b)
		    where
			(sf1', b) = (sfTF' sf1) dt a
-}

cpEX :: (Event a -> b) -> b -> SF' b c -> SF' (Event a) c
cpEX f1 f1ne sf2 = cpEXAux (FDE f1 f1ne) f1 f1ne sf2
    where
	cpEXAux :: FunDesc (Event a) b -> (Event a -> b) -> b 
                   -> SF' b c -> SF' (Event a) c
	cpEXAux fd1 _ _ (SFArr _ fd2) = sfArr (fdComp fd1 fd2)
        cpEXAux _ f1 _   (SFSScan _ f s c) = sfSScan (\s a -> f s (f1 a)) s c
        -- We must not capture cne in the f closure since cne can change!
        -- See cpXX the SFEP/SFEP case for a similar situation. However,
        -- FDE represent a state-less signal function, so *its* NoEvent
        -- value never changes. Hence we only need to verify that it is
        -- NoEvent once.
	cpEXAux _ f1 f1ne (SFEP _ f2 s cne) =
	    sfEP f (s, cne) (vfyNoEv f1ne cne)
            where
                f scne@(s, cne) a =
                    case (f1 (Event a)) of
                        NoEvent -> (scne, cne, cne)
                        Event b ->
                            let (s', c, cne') = f2 s b in ((s', cne'), c, cne')
	cpEXAux fd1 _ _ (SFCpAXA _ fd21 sf22 fd23) =
            cpAXA (fdComp fd1 fd21) sf22 fd23
        -- The rationale for the following is that the case analysis
	-- is typically not going to be more expensive than applying
	-- the function and possibly a bit cheaper. Thus if events
	-- are sparse, we might win, and if not, we don't loose to
	-- much.
	cpEXAux fd1 f1 f1ne sf2 = SFCpAXA tf fd1 sf2 FDI
{-
	    if sfIsInv sf2 then
		cpEXInv fd1 f1 f1ne sf2
	    else
	    	SFCpAXA tf False fd1 sf2 FDI
-}
	    where
		tf dt ea = (cpEXAux fd1 f1 f1ne sf2', c)
		    where
                        (sf2', c) =
			    case ea of
				NoEvent -> (sfTF' sf2) dt f1ne
				_       -> (sfTF' sf2) dt (f1 ea)

{-
	cpEXInv fd1 f1 f1ne sf2 = SFCpAXA tf True fd1 sf2 FDI
	    where
		tf dt ea = sf2 `seq` (cpEXInv fd1 f1 f1ne sf2', c)
		    where
                        (sf2', c) =
			    case ea of
				NoEvent -> (sfTF' sf2) dt f1ne
				_       -> (sfTF' sf2) dt (f1 ea)
-}

cpXE :: SF' a (Event b) -> (Event b -> c) -> c -> SF' a c
cpXE sf1 f2 f2ne = cpXEAux (FDE f2 f2ne) f2 f2ne sf1
    where
	cpXEAux :: FunDesc (Event b) c -> (Event b -> c) -> c
		   -> SF' a (Event b) -> SF' a c
        cpXEAux fd2 _ _ (SFArr _ fd1) = sfArr (fdComp fd1 fd2)
        cpXEAux _ f2 f2ne (SFSScan _ f s eb) = sfSScan f' s (f2 eb)
            where
	        f' s a = case f s a of
                             Nothing -> Nothing
                             Just (s', NoEvent) -> Just (s', f2ne) 
                             Just (s', eb')     -> Just (s', f2 eb') 
        cpXEAux _ f2 f2ne (SFEP _ f1 s ebne) =
	    sfEP f s (vfyNoEv ebne f2ne)
            where
                f s a =
                    case f1 s a of
                        (s', NoEvent, NoEvent) -> (s', f2ne,  f2ne)
                        (s', eb,      NoEvent) -> (s', f2 eb, f2ne)
		        _ -> usrErr "AFRP" "cpXEAux" "Assertion failed: Functions on events must not map NoEvent to Event."
        cpXEAux fd2 _ _ (SFCpAXA _ fd11 sf12 fd13) =
            cpAXA fd11 sf12 (fdComp fd13 fd2)
	cpXEAux fd2 f2 f2ne sf1 = SFCpAXA tf FDI sf1 fd2
{-
	    if sfIsInv sf1 then
		cpXEInv fd2 f2 f2ne sf1
	    else
		SFCpAXA tf False FDI sf1 fd2
-}
	    where
		tf dt a = (cpXEAux fd2 f2 f2ne sf1',
                           case eb of NoEvent -> f2ne; _ -> f2 eb)
		    where
                        (sf1', eb) = (sfTF' sf1) dt a

{-
	cpXEInv fd2 f2 f2ne sf1 = SFCpAXA tf True FDI sf1 fd2
	    where
		tf dt a = sf1 `seq` (cpXEInv fd2 f2 f2ne sf1',
                           case eb of NoEvent -> f2ne; _ -> f2 eb)
		    where
                        (sf1', eb) = (sfTF' sf1) dt a
-}
	

-- Widening.
-- The definition exploits the following identities:
--     first identity     = identity				-- New
--     first (constant b) = arr (\(_, c) -> (b, c))
--     (first (arr f))    = arr (\(a, c) -> (f a, c))
firstPrim :: SF a b -> SF (a,c) (b,c)
firstPrim (SF {sfTF = tf10}) = SF {sfTF = tf0}
    where
        tf0 ~(a0, c0) = (fpAux sf1, (b0, c0))
	    where
		(sf1, b0) = tf10 a0 


-- Also used in parSplitPrim
fpAux :: SF' a b -> SF' (a,c) (b,c)
fpAux (SFArr _ FDI)       = sfId			-- New
fpAux (SFArr _ (FDC b))   = sfArrG (\(~(_, c)) -> (b, c))
fpAux (SFArr _ fd1)       = sfArrG (\(~(a, c)) -> ((fdFun fd1) a, c))
fpAux sf1 = SF' tf
    -- if sfIsInv sf1 then fpInv sf1 else SF' tf False
    where
        tf dt ~(a, c) = (fpAux sf1', (b, c))
	    where
		(sf1', b) = (sfTF' sf1) dt a 


{-
fpInv :: SF' a b -> SF' (a,c) (b,c)
fpInv sf1 = SF' tf True
    where
        tf dt ~(a, c) = sf1 `seq` (fpInv sf1', (b, c))
	    where
		(sf1', b) = (sfTF' sf1) dt a 
-}


-- Mirror image of first.
secondPrim :: SF a b -> SF (c,a) (c,b)
secondPrim (SF {sfTF = tf10}) = SF {sfTF = tf0}
    where
        tf0 ~(c0, a0) = (spAux sf1, (c0, b0))
	    where
		(sf1, b0) = tf10 a0 


-- Also used in parSplitPrim
spAux :: SF' a b -> SF' (c,a) (c,b)
spAux (SFArr _ FDI)       = sfId			-- New
spAux (SFArr _ (FDC b))   = sfArrG (\(~(c, _)) -> (c, b))
spAux (SFArr _ fd1)       = sfArrG (\(~(c, a)) -> (c, (fdFun fd1) a))
spAux sf1 = SF' tf
    -- if sfIsInv sf1 then spInv sf1 else SF' tf False
    where
        tf dt ~(c, a) = (spAux sf1', (c, b))
	    where
		(sf1', b) = (sfTF' sf1) dt a 


{-
spInv :: SF' a b -> SF' (c,a) (c,b)
spInv sf1 = SF' tf True
    where
        tf dt ~(c, a) = sf1 `seq` (spInv sf1', (c, b))
	    where
		(sf1', b) = (sfTF' sf1) dt a 
-}


-- Parallel composition.
-- The definition exploits the following identities (that hold for SF):
--     identity   *** identity   = identity		-- New
--     sf         *** identity   = first sf		-- New
--     identity   *** sf         = second sf		-- New
--     constant b *** constant d = constant (b, d)
--     constant b *** arr f2     = arr (\(_, c) -> (b, f2 c)
--     arr f1     *** constant d = arr (\(a, _) -> (f1 a, d)
--     arr f1     *** arr f2     = arr (\(a, b) -> (f1 a, f2 b)
parSplitPrim :: SF a b -> SF c d  -> SF (a,c) (b,d)
parSplitPrim (SF {sfTF = tf10}) (SF {sfTF = tf20}) = SF {sfTF = tf0}
    where
	tf0 ~(a0, c0) = (psXX sf1 sf2, (b0, d0))
	    where
		(sf1, b0) = tf10 a0 
		(sf2, d0) = tf20 c0 

	-- Naming convention: ps<X><Y> where  <X> and <Y> is one of:
        -- X - arbitrary signal function
        -- A - arbitrary pure arrow
        -- C - constant arrow

        psXX :: SF' a b -> SF' c d -> SF' (a,c) (b,d)
        psXX (SFArr _ fd1)       (SFArr _ fd2)       = sfArr (fdPar fd1 fd2)
        psXX (SFArr _ FDI)       sf2                 = spAux sf2	-- New
	psXX (SFArr _ (FDC b))   sf2                 = psCX b sf2
	psXX (SFArr _ fd1)       sf2                 = psAX (fdFun fd1) sf2
        psXX sf1                 (SFArr _ FDI)       = fpAux sf1	-- New
	psXX sf1                 (SFArr _ (FDC d))   = psXC sf1 d
	psXX sf1                 (SFArr _ fd2)       = psXA sf1 (fdFun fd2)
-- !!! Unclear if this really is a gain.
-- !!! potentially unnecessary tupling and untupling.
-- !!! To be investigated.
-- !!! 2005-07-01: At least for MEP 6, the corresponding opt for
-- !!! &&& was harmfull. On that basis, disable it here too.
--        psXX (SFCpAXA _ fd11 sf12 fd13) (SFCpAXA _ fd21 sf22 fd23) =
--            cpAXA (fdPar fd11 fd21) (psXX sf12 sf22) (fdPar fd13 fd23)
	psXX sf1 sf2 = SF' tf
{-
	    if sfIsInv sf1 && sfIsInv sf2 then
		psXXInv sf1 sf2
	    else
		SF' tf False
-}
	    where
		tf dt ~(a, c) = (psXX sf1' sf2', (b, d))
		    where
		        (sf1', b) = (sfTF' sf1) dt a
			(sf2', d) = (sfTF' sf2) dt c

{-
        psXXInv :: SF' a b -> SF' c d -> SF' (a,c) (b,d)
	psXXInv sf1 sf2 = SF' tf True
	    where
		tf dt ~(a, c) = sf1 `seq` sf2 `seq` (psXXInv sf1' sf2',
                                                       (b, d))
		    where
		        (sf1', b) = (sfTF' sf1) dt a
			(sf2', d) = (sfTF' sf2) dt c
-}

        psCX :: b -> SF' c d -> SF' (a,c) (b,d)
	psCX b (SFArr _ fd2)       = sfArr (fdPar (FDC b) fd2)
	psCX b sf2                 = SF' tf
{-
	    if sfIsInv sf2 then
	        psCXInv b sf2
	    else
	        SF' tf False
-}
	    where
		tf dt ~(_, c) = (psCX b sf2', (b, d))
		    where
			(sf2', d) = (sfTF' sf2) dt c

{-
        psCXInv :: b -> SF' c d -> SF' (a,c) (b,d)
	psCXInv b sf2 = SF' tf True
	    where
		tf dt ~(_, c) = sf2 `seq` (psCXInv b sf2', (b, d))
		    where
			(sf2', d) = (sfTF' sf2) dt c
-}

        psXC :: SF' a b -> d -> SF' (a,c) (b,d)
        psXC (SFArr _ fd1)       d = sfArr (fdPar fd1 (FDC d))
	psXC sf1                 d = SF' tf
{-
	    if sfIsInv sf1 then
		psXCInv sf1 d
	    else
                SF' tf False
-}
	    where
		tf dt ~(a, _) = (psXC sf1' d, (b, d))
		    where
			(sf1', b) = (sfTF' sf1) dt a

{-
        psXCInv :: SF' a b -> d -> SF' (a,c) (b,d)
	psXCInv sf1 d = SF' tf True
	    where
		tf dt ~(a, _) = sf1 `seq` (psXCInv sf1' d, (b, d))
		    where
			(sf1', b) = (sfTF' sf1) dt a
-}

        psAX :: (a -> b) -> SF' c d -> SF' (a,c) (b,d)
	psAX f1 (SFArr _ fd2)       = sfArr (fdPar (FDG f1) fd2)
	psAX f1 sf2                 = SF' tf
{-
	    if sfIsInv sf2 then
	    	psAXInv f1 sf2
	    else
                SF' tf False
-}
	    where
		tf dt ~(a, c) = (psAX f1 sf2', (f1 a, d))
		    where
			(sf2', d) = (sfTF' sf2) dt c

{-
        psAXInv :: (a -> b) -> SF' c d -> SF' (a,c) (b,d)
	psAXInv f1 sf2 = SF' tf True
	    where
		tf dt ~(a, c) = sf2 `seq` (psAXInv f1 sf2', (f1 a, d))
		    where
			(sf2', d) = (sfTF' sf2) dt c
-}

        psXA :: SF' a b -> (c -> d) -> SF' (a,c) (b,d)
	psXA (SFArr _ fd1)       f2 = sfArr (fdPar fd1 (FDG f2))
	psXA sf1                 f2 = SF' tf
{-
	    if sfIsInv sf1 then
		psXAInv sf1 f2 
	    else
		SF' tf False
-}
	    where
		tf dt ~(a, c) = (psXA sf1' f2, (b, f2 c))
		    where
			(sf1', b) = (sfTF' sf1) dt a

{-
        psXAInv :: SF' a b -> (c -> d) -> SF' (a,c) (b,d)
	psXAInv sf1 f2 = SF' tf True
	    where
		tf dt ~(a, c) = sf1 `seq` (psXAInv sf1' f2, (b, f2 c))
		    where
			(sf1', b) = (sfTF' sf1) dt a
-}


-- !!! Hmmm. Why don't we optimize the FDE cases here???
-- !!! Seems pretty obvious that we should!
-- !!! It should also be possible to optimize an event processor in
-- !!! parallel with another event processor or an Arr FDE.

parFanOutPrim :: SF a b -> SF a c -> SF a (b, c)
parFanOutPrim (SF {sfTF = tf10}) (SF {sfTF = tf20}) = SF {sfTF = tf0}
    where
	tf0 a0 = (pfoXX sf1 sf2, (b0, c0))
	    where
		(sf1, b0) = tf10 a0 
		(sf2, c0) = tf20 a0 

	-- Naming convention: pfo<X><Y> where  <X> and <Y> is one of:
        -- X - arbitrary signal function
        -- A - arbitrary pure arrow
        -- I - identity arrow
        -- C - constant arrow

        pfoXX :: SF' a b -> SF' a c -> SF' a (b ,c)
        pfoXX (SFArr _ fd1)       (SFArr _ fd2)       = sfArr(fdFanOut fd1 fd2)
        pfoXX (SFArr _ FDI)       sf2                 = pfoIX sf2
	pfoXX (SFArr _ (FDC b))   sf2                 = pfoCX b sf2
	pfoXX (SFArr _ fd1)       sf2                 = pfoAX (fdFun fd1) sf2
        pfoXX sf1                 (SFArr _ FDI)       = pfoXI sf1
	pfoXX sf1                 (SFArr _ (FDC c))   = pfoXC sf1 c
	pfoXX sf1                 (SFArr _ fd2)       = pfoXA sf1 (fdFun fd2)
-- !!! Unclear if this really would be a gain
-- !!! 2005-07-01: NOT a win for MEP 6.
--        pfoXX (SFCpAXA _ fd11 sf12 fd13) (SFCpAXA _ fd21 sf22 fd23) =
--            cpAXA (fdPar fd11 fd21) (psXX sf12 sf22) (fdPar fd13 fd23)
	pfoXX sf1 sf2 = SF' tf
{-
	    if sfIsInv sf1 && sfIsInv sf2 then
		pfoXXInv sf1 sf2
	    else
		SF' tf False
-}
	    where
		tf dt a = (pfoXX sf1' sf2', (b, c))
		    where
		        (sf1', b) = (sfTF' sf1) dt a
			(sf2', c) = (sfTF' sf2) dt a

{-
        pfoXXInv :: SF' a b -> SF' a c -> SF' a (b ,c)
	pfoXXInv sf1 sf2 = SF' tf True
	    where
		tf dt a = sf1 `seq` sf2 `seq` (pfoXXInv sf1' sf2', (b, c))
		    where
		        (sf1', b) = (sfTF' sf1) dt a
			(sf2', c) = (sfTF' sf2) dt a
-}

        pfoIX :: SF' a c -> SF' a (a ,c)
	pfoIX (SFArr _ fd2) = sfArr (fdFanOut FDI fd2)
	pfoIX sf2 = SF' tf
{-
	    if sfIsInv sf2 then
		pfoIXInv sf2
	    else
		SF' tf False
-}
	    where
		tf dt a = (pfoIX sf2', (a, c))
		    where
			(sf2', c) = (sfTF' sf2) dt a

{-
        pfoIXInv :: SF' a c -> SF' a (a ,c)
	pfoIXInv sf2 = SF' tf True
	    where
		tf dt a = sf2 `seq` (pfoIXInv sf2', (a, c))
		    where
			(sf2', c) = (sfTF' sf2) dt a
-}

        pfoXI :: SF' a b -> SF' a (b ,a)
	pfoXI (SFArr _ fd1) = sfArr (fdFanOut fd1 FDI)
	pfoXI sf1 = SF' tf
{-
            if sfIsInv sf1 then
		pfoXIInv sf1
	    else
		SF' tf False
-}
	    where
		tf dt a = (pfoXI sf1', (b, a))
		    where
			(sf1', b) = (sfTF' sf1) dt a

{-
        pfoXIInv :: SF' a b -> SF' a (b ,a)
	pfoXIInv sf1 = SF' tf True
	    where
		tf dt a = sf1 `seq` (pfoXIInv sf1', (b, a))
		    where
			(sf1', b) = (sfTF' sf1) dt a
-}

        pfoCX :: b -> SF' a c -> SF' a (b ,c)
        pfoCX b (SFArr _ fd2) = sfArr (fdFanOut (FDC b) fd2)
	pfoCX b sf2 = SF' tf
{-
	    if sfIsInv sf2 then
		pfoCXInv b sf2
	    else
		SF' tf False
-}
	    where
		tf dt a = (pfoCX b sf2', (b, c))
		    where
			(sf2', c) = (sfTF' sf2) dt a

{-
        pfoCXInv :: b -> SF' a c -> SF' a (b ,c)
	pfoCXInv b sf2 = SF' tf True
	    where
		tf dt a = sf2 `seq` (pfoCXInv b sf2', (b, c))
		    where
			(sf2', c) = (sfTF' sf2) dt a
-}

        pfoXC :: SF' a b -> c -> SF' a (b ,c)
	pfoXC (SFArr _ fd1) c = sfArr (fdFanOut fd1 (FDC c))
	pfoXC sf1 c = SF' tf
{-
	    if sfIsInv sf1 then
		pfoXCInv sf1 c
	    else
	        SF' tf False
-}
	    where
		tf dt a = (pfoXC sf1' c, (b, c))
		    where
			(sf1', b) = (sfTF' sf1) dt a

{-
        pfoXCInv :: SF' a b -> c -> SF' a (b ,c)
	pfoXCInv sf1 c = SF' tf True
	    where
		tf dt a = sf1 `seq` (pfoXCInv sf1' c, (b, c))
		    where
			(sf1', b) = (sfTF' sf1) dt a
-}

        pfoAX :: (a -> b) -> SF' a c -> SF' a (b ,c)
	pfoAX f1 (SFArr _ fd2) = sfArr (fdFanOut (FDG f1) fd2)
	pfoAX f1 sf2 = SF' tf
{-
	    if sfIsInv sf2 then
		pfoAXInv f1 sf2
	    else
                SF' tf False
-}
	    where
		tf dt a = (pfoAX f1 sf2', (f1 a, c))
		    where
			(sf2', c) = (sfTF' sf2) dt a

{-
        pfoAXInv :: (a -> b) -> SF' a c -> SF' a (b ,c)
	pfoAXInv f1 sf2 = SF' tf True
	    where
		tf dt a = sf2 `seq` (pfoAXInv f1 sf2', (f1 a, c))
		    where
			(sf2', c) = (sfTF' sf2) dt a
-}

        pfoXA :: SF' a b -> (a -> c) -> SF' a (b ,c)
	pfoXA (SFArr _ fd1) f2 = sfArr (fdFanOut fd1 (FDG f2))
	pfoXA sf1 f2 = SF' tf
{-
	    if sfIsInv sf1 then
		pfoXAInv sf1 f2
	    else
		SF' tf False
-}
	    where
		tf dt a = (pfoXA sf1' f2, (b, f2 a))
		    where
			(sf1', b) = (sfTF' sf1) dt a

{-
        pfoXAInv :: SF' a b -> (a -> c) -> SF' a (b ,c)
	pfoXAInv sf1 f2 = SF' tf True
	    where
		tf dt a = sf1 `seq` (pfoXAInv sf1' f2, (b, f2 a))
		    where
			(sf1', b) = (sfTF' sf1) dt a
-}


------------------------------------------------------------------------------
-- ArrowLoop instance and implementation
------------------------------------------------------------------------------

instance ArrowLoop SF where
    loop = loopPrim


loopPrim :: SF (a,c) (b,c) -> SF a b
loopPrim (SF {sfTF = tf10}) = SF {sfTF = tf0}
    where
	tf0 a0 = (loopAux sf1, b0)
	    where
	        (sf1, (b0, c0)) = tf10 (a0, c0)

        loopAux :: SF' (a,c) (b,c) -> SF' a b
	loopAux (SFArr _ FDI) = sfId
        loopAux (SFArr _ (FDC (b, _))) = sfConst b
	loopAux (SFArr _ fd1) =
            sfArrG (\a -> let (b,c) = (fdFun fd1) (a,c) in b)
	loopAux sf1 = SF' tf
{-
	    if sfIsInv sf1 then
		loopInv sf1
	    else
		SF' tf False
-}
	    where
		tf dt a = (loopAux sf1', b)
		    where
		        (sf1', (b, c)) = (sfTF' sf1) dt (a, c)

{-
        loopInv :: SF' (a,c) (b,c) -> SF' a b
	loopInv sf1 = SF' tf True
	    where
		tf dt a = sf1 `seq` (loopInv sf1', b)
		    where
		        (sf1', (b, c)) = (sfTF' sf1) dt (a, c)
-}


------------------------------------------------------------------------------
-- Basic signal functions
------------------------------------------------------------------------------

-- Identity: identity = arr id
identity :: SF a a
identity = SF {sfTF = \a -> (sfId, a)}


-- Identity: constant b = arr (const b)
constant :: b -> SF a b
constant b = SF {sfTF = \_ -> (sfConst b, b)}


-- Outputs the time passed since the signal function instance was started.
localTime :: SF a Time
localTime = constant 1.0 >>> integral


-- Alternative name for localTime.
time :: SF a Time
time = localTime


------------------------------------------------------------------------------
-- Initialization
------------------------------------------------------------------------------

-- Initialization operator (cf. Lustre/Lucid Synchrone).
(-->) :: b -> SF a b -> SF a b
b0 --> (SF {sfTF = tf10}) = SF {sfTF = \a0 -> (fst (tf10 a0), b0)}


-- Input initialization operator.
(>--) :: a -> SF a b -> SF a b
a0 >-- (SF {sfTF = tf10}) = SF {sfTF = \_ -> tf10 a0}


-- Transform initial output value.
(-=>) :: (b -> b) -> SF a b -> SF a b
f -=> (SF {sfTF = tf10}) =
    SF {sfTF = \a0 -> let (sf1, b0) = tf10 a0 in (sf1, f b0)}


-- Transform initial input value.
(>=-) :: (a -> a) -> SF a b -> SF a b
f >=- (SF {sfTF = tf10}) = SF {sfTF = \a0 -> tf10 (f a0)}


-- Override initial value of input signal.
initially :: a -> SF a a
initially = (--> identity)


------------------------------------------------------------------------------
-- Simple, stateful signal processing
------------------------------------------------------------------------------

-- New sscan primitive. It should be possible to define lots of functions
-- in terms of this one. Eventually a new constructor will be introduced if
-- this works out.

sscan :: (b -> a -> b) -> b -> SF a b
sscan f b_init = sscanPrim f' b_init b_init
    where
        f' b a = let b' = f b a in Just (b', b')


{-
sscanPrim :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b
sscanPrim f c_init b_init = SF {sfTF = tf0}
    where
        tf0 a0 = case f c_init a0 of
                     Nothing       -> (spAux f c_init b_init, b_init)
                     Just (c', b') -> (spAux f c' b', b')
 
        spAux :: (c -> a -> Maybe (c, b)) -> c -> b -> SF' a b
        spAux f c b = sf
            where
                -- sf = SF' tf True
                sf = SF' tf
                tf _ a = case f c a of
                             Nothing       -> (sf, b)
                             Just (c', b') -> (spAux f c' b', b')
-}


------------------------------------------------------------------------------
-- Basic event sources
------------------------------------------------------------------------------

-- Event source that never occurs.
never :: SF a (Event b)
never = SF {sfTF = \_ -> (sfNever, NoEvent)}


-- Event source with a single occurrence at time 0. The value of the event
-- is given by the function argument.
now :: b -> SF a (Event b)
now b0 = (Event b0 --> never)


-- Event source with a single occurrence at or as soon after (local) time q
-- as possible.
after :: Time -> b -> SF a (Event b)
after q x = afterEach [(q,x)]


-- Event source with repeated occurrences with interval q.
-- Note: If the interval is too short w.r.t. the sampling intervals,
-- the result will be that events occur at every sample. However, no more
-- than one event results from any sampling interval, thus avoiding an
-- "event backlog" should sampling become more frequent at some later
-- point in time.
-- !!! 2005-03-30:  This is potentially a bit inefficient since we KNOW
-- !!! (at this level) that the SF is going to be invarying. But afterEach
-- !!! does NOT know this as the argument list may well be finite.
-- !!! We could use sfMkInv, but that's not without problems.
-- !!! We're probably better off specializing afterEachCat here.

repeatedly :: Time -> b -> SF a (Event b)
repeatedly q x | q > 0 = afterEach qxs
               | otherwise = usrErr "AFRP" "repeatedly" "Non-positive period."
    where
        qxs = (q,x):qxs        


-- Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- only the first will in fact occur to avoid an event backlog.
-- Question: Should positive periods except for the first one be required?
-- Note that periods of length 0 will always be skipped except for the first.
-- Right now, periods of length 0 is allowed on the grounds that no attempt
-- is made to forbid simultaneous events elsewhere.
{-
afterEach :: [(Time,b)] -> SF a (Event b)
afterEach [] = never
afterEach ((q,x):qxs)
    | q < 0     = usrErr "AFRP" "afterEach" "Negative period."
    | otherwise = SF {sfTF = tf0}
    where
	tf0 _ = if q <= 0 then
                    (scheduleNextEvent 0.0 qxs, Event x)
                else
		    (awaitNextEvent (-q) x qxs, NoEvent)

	scheduleNextEvent t [] = sfNever
        scheduleNextEvent t ((q,x):qxs)
	    | q < 0     = usrErr "AFRP" "afterEach" "Negative period."
	    | t' >= 0   = scheduleNextEvent t' qxs
	    | otherwise = awaitNextEvent t' x qxs
	    where
	        t' = t - q
	awaitNextEvent t x qxs = SF' {sfTF' = tf}
	    where
		tf dt _ | t' >= 0   = (scheduleNextEvent t' qxs, Event x)
		        | otherwise = (awaitNextEvent t' x qxs, NoEvent)
		    where
		        t' = t + dt
-}

-- Or keep old def. for efficiency reasons?
-- After all, after, repeatedly etc. are defined in terms of afterEach.
afterEach :: [(Time,b)] -> SF a (Event b)
afterEach qxs = afterEachCat qxs >>> arr (fmap head)


-- Guaranteed not to miss any events.
afterEachCat :: [(Time,b)] -> SF a (Event [b])
afterEachCat [] = never
afterEachCat ((q,x):qxs)
    | q < 0     = usrErr "AFRP" "afterEachCat" "Negative period."
    | otherwise = SF {sfTF = tf0}
    where
	tf0 _ = if q <= 0 then
                    emitEventsScheduleNext 0.0 [x] qxs
                else
		    (awaitNextEvent (-q) x qxs, NoEvent)

	emitEventsScheduleNext _ xs [] = (sfNever, Event (reverse xs))
        emitEventsScheduleNext t xs ((q,x):qxs)
	    | q < 0     = usrErr "AFRP" "afterEachCat" "Negative period."
	    | t' >= 0   = emitEventsScheduleNext t' (x:xs) qxs
	    | otherwise = (awaitNextEvent t' x qxs, Event (reverse xs))
	    where
	        t' = t - q
	awaitNextEvent t x qxs = SF' tf -- False
	    where
		tf dt _ | t' >= 0   = emitEventsScheduleNext t' [x] qxs
		        | otherwise = (awaitNextEvent t' x qxs, NoEvent)
		    where
		        t' = t + dt

-- Delay for events. (Consider it a triggered after, hence "basic".)
-- Can be implemented fairly cheaply as long as the events are sparse.
-- It is a question of rescheduling events for later. Not unlike "afterEach".
--
-- It is not exactly the case that delayEvent t = delay t NoEvent
-- since the rules for dropping/extrapolating samples are different.
-- A single event occurrence will never be duplicated.
-- If there is an event occurrence, one will be output as soon as
-- possible after the given delay time, but not necessarily that
-- one.  See delayEventCat.

delayEvent :: Time -> SF (Event a) (Event a)
delayEvent q | q < 0     = usrErr "AFRP" "delayEvent" "Negative delay."
             | q == 0    = identity
             | otherwise = delayEventCat q >>> arr (fmap head)


-- There is no *guarantee* above that every event actually will be
-- rescheduled since the sampling frequency (temporarily) might drop.
-- The following interface would allow ALL scheduled events to occur
-- as soon as possible:
-- (Read "delay event and catenate events that occur so closely so as to be
-- inseparable".)
-- The events in the list are ordered temporally to the extent possible.

{-
-- This version is too strict!
delayEventCat :: Time -> SF (Event a) (Event [a])
delayEventCat q | q < 0     = usrErr "AFRP" "delayEventCat" "Negative delay."
                | q == 0    = arr (fmap (:[]))
                | otherwise = SF {sfTF = tf0}
    where
	tf0 NoEvent   = (noPendingEvent, NoEvent)
        tf0 (Event x) = (pendingEvents (-q) [] [] (-q) x, NoEvent)

        noPendingEvent = SF' tf -- True
            where
                tf _ NoEvent   = (noPendingEvent, NoEvent)
                tf _ (Event x) = (pendingEvents (-q) [] [] (-q) x, NoEvent)
				 
        -- t_next is the present time w.r.t. the next scheduled event.
        -- t_last is the present time w.r.t. the last scheduled event.
        -- In the event queues, events are associated with their time
	-- w.r.t. to preceding event (positive).
        pendingEvents t_last rqxs qxs t_next x = SF' tf -- True
            where
	        tf dt NoEvent    = tf1 (t_last + dt) rqxs (t_next + dt)
                tf dt (Event x') = tf1 (-q) ((q', x') : rqxs) t_next'
		    where
		        t_next' = t_next  + dt
                        t_last' = t_last  + dt
                        q'      = t_last' + q

                tf1 t_last' rqxs' t_next'
                    | t_next' >= 0 =
                        emitEventsScheduleNext t_last' rqxs' qxs t_next' [x]
		    | otherwise =
                        (pendingEvents t_last' rqxs' qxs t_next' x, NoEvent)

        -- t_next is the present time w.r.t. the *scheduled* time of the
        -- event that is about to be emitted (i.e. >= 0).
        -- The time associated with any event at the head of the event
        -- queue is also given w.r.t. the event that is about to be emitted.
        -- Thus, t_next - q' is the present time w.r.t. the event at the head
        -- of the event queue.
        emitEventsScheduleNext t_last [] [] t_next rxs =
            (noPendingEvent, Event (reverse rxs))
        emitEventsScheduleNext t_last rqxs [] t_next rxs =
            emitEventsScheduleNext t_last [] (reverse rqxs) t_next rxs
        emitEventsScheduleNext t_last rqxs ((q', x') : qxs') t_next rxs
            | q' > t_next = (pendingEvents t_last rqxs qxs' (t_next - q') x',
                             Event (reverse rxs))
            | otherwise   = emitEventsScheduleNext t_last rqxs qxs' (t_next-q')
                                                   (x' : rxs)
-}

-- This version is not strict in the input event.
delayEventCat :: Time -> SF (Event a) (Event [a])
delayEventCat q | q < 0     = usrErr "AFRP" "delayEventCat" "Negative delay."
                | q == 0    = arr (fmap (:[]))
                | otherwise = SF {sfTF = tf0}
    where
        tf0 e = (case e of
                     NoEvent -> noPendingEvent
                     Event x -> pendingEvents (-q) [] [] (-q) x,
                 NoEvent)

        noPendingEvent = SF' tf -- True
            where
                tf _ e = (case e of
                              NoEvent -> noPendingEvent
                              Event x -> pendingEvents (-q) [] [] (-q) x,
                          NoEvent)
				 
        -- t_next is the present time w.r.t. the next scheduled event.
        -- t_last is the present time w.r.t. the last scheduled event.
        -- In the event queues, events are associated with their time
	-- w.r.t. to preceding event (positive).
        pendingEvents t_last rqxs qxs t_next x = SF' tf -- True
            where
                tf dt e
                    | t_next' >= 0 =
			emitEventsScheduleNext e t_last' rqxs qxs t_next' [x]
                    | otherwise    = 
			(pendingEvents t_last'' rqxs' qxs t_next' x, NoEvent)
                    where
		        t_next' = t_next  + dt
                        t_last' = t_last  + dt 
                        (t_last'', rqxs') =
                            case e of
                                NoEvent  -> (t_last', rqxs)
                                Event x' -> (-q, (t_last'+q,x') : rqxs)

        -- t_next is the present time w.r.t. the *scheduled* time of the
        -- event that is about to be emitted (i.e. >= 0).
        -- The time associated with any event at the head of the event
        -- queue is also given w.r.t. the event that is about to be emitted.
        -- Thus, t_next - q' is the present time w.r.t. the event at the head
        -- of the event queue.
        emitEventsScheduleNext e _ [] [] _ rxs =
            (case e of
                 NoEvent -> noPendingEvent
                 Event x -> pendingEvents (-q) [] [] (-q) x, 
             Event (reverse rxs))
        emitEventsScheduleNext e t_last rqxs [] t_next rxs =
            emitEventsScheduleNext e t_last [] (reverse rqxs) t_next rxs
        emitEventsScheduleNext e t_last rqxs ((q', x') : qxs') t_next rxs
            | q' > t_next = (case e of
                                 NoEvent -> 
				     pendingEvents t_last 
                                                   rqxs 
                                                   qxs'
                                                   (t_next - q')
                                                   x'
                                 Event x'' ->
				     pendingEvents (-q) 
                                                   ((t_last+q, x'') : rqxs)
                                                   qxs'
                                                   (t_next - q')
                                                   x',
                             Event (reverse rxs))
            | otherwise   = emitEventsScheduleNext e
                                                   t_last
                                                   rqxs 
                                                   qxs' 
                                                   (t_next - q')
                                                   (x' : rxs)


-- A rising edge detector. Useful for things like detecting key presses.
-- Note that we initialize the loop with state set to True so that there
-- will not be an occurence at t0 in the logical time frame in which
-- this is started.
edge :: SF Bool (Event ())
edge = iEdge True


iEdge :: Bool -> SF Bool (Event ())
-- iEdge i = edgeBy (isBoolRaisingEdge ()) i
iEdge b = sscanPrim f (if b then 2 else 0) NoEvent
    where
        f :: Int -> Bool -> Maybe (Int, Event ())
        f 0 False = Nothing
        f 0 True  = Just (1, Event ())
        f 1 False = Just (0, NoEvent)
        f 1 True  = Just (2, NoEvent)
        f 2 False = Just (0, NoEvent)
        f 2 True  = Nothing
        f _ _     = undefined

-- Like edge, but parameterized on the tag value.
edgeTag :: a -> SF Bool (Event a)
-- edgeTag a = edgeBy (isBoolRaisingEdge a) True
edgeTag a = edge >>> arr (`tag` a)


-- Internal utility.
-- isBoolRaisingEdge :: a -> Bool -> Bool -> Maybe a
-- isBoolRaisingEdge _ False False = Nothing
-- isBoolRaisingEdge a False True  = Just a
-- isBoolRaisingEdge _ True  True  = Nothing
-- isBoolRaisingEdge _ True  False = Nothing


-- !!! 2005-07-09: To be done or eliminated
-- !!! Maybe could be kept as is, but could be easy to implement directly
-- !!! in terms of sscan?
edgeJust :: SF (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
    where
        isJustEdge Nothing  Nothing     = Nothing
        isJustEdge Nothing  ma@(Just _) = ma
        isJustEdge (Just _) (Just _)    = Nothing
        isJustEdge (Just _) Nothing     = Nothing


-- Edge detector parameterized on the edge detection function and initial
-- state, i.e., the previous input sample. The first argument to the
-- edge detection function is the previous sample, the second the current one.

-- !!! Is this broken!?! Does not disallow an edge condition that persists
-- !!! between consecutive samples. See discussion in ToDo list above.
-- !!! 2005-07-09: To be done.
edgeBy :: (a -> a -> Maybe b) -> a -> SF a (Event b)
edgeBy isEdge a_init = SF {sfTF = tf0}
    where
	tf0 a0 = (ebAux a0, maybeToEvent (isEdge a_init a0))

	ebAux a_prev = SF' tf -- True
	    where
		tf _ a = (ebAux a, maybeToEvent (isEdge a_prev a))


------------------------------------------------------------------------------
-- Stateful event suppression
------------------------------------------------------------------------------

-- Suppression of initial (at local time 0) event.
notYet :: SF (Event a) (Event a)
notYet = initially NoEvent


-- Suppress all but first event.
once :: SF (Event a) (Event a)
once = takeEvents 1


-- Suppress all but first n events.
takeEvents :: Int -> SF (Event a) (Event a)
takeEvents n | n <= 0 = never
takeEvents n = dSwitch (arr dup) (const (NoEvent >-- takeEvents (n - 1)))


{-
-- More complicated using "switch" that "dSwitch".
takeEvents :: Int -> SF (Event a) (Event a)
takeEvents 0       = never
takeEvents (n + 1) = switch (never &&& identity) (takeEvents' n)
    where
        takeEvents' 0       a = now a
        takeEvents' (n + 1) a = switch (now a &&& notYet) (takeEvents' n)
-}


-- Suppress first n events.
-- Here dSwitch or switch does not really matter.
dropEvents :: Int -> SF (Event a) (Event a)
dropEvents n | n <= 0  = identity
dropEvents n = dSwitch (never &&& identity)
                             (const (NoEvent >-- dropEvents (n - 1)))


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

-- Basic switch.
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


-- Switch with delayed observation.
-- Or "decoupled switch"?
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


-- Recurring switch.
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


-- Recurring switch with delayed observation.
drSwitch :: SF a b -> SF (a, Event (SF a b)) b
drSwitch sf = dSwitch (first sf) ((noEventSnd >=-) . drSwitch)

{-
-- Old version. New is more efficient. Which one is clearer?
drSwitch :: SF a b -> SF (a, Event (SF a b)) b
drSwitch sf = dSwitch (first sf) drSwitch'
    where
        drSwitch' sf = dSwitch (sf *** notYet) drSwitch'
-}


-- "Call-with-current-continuation" switch.
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


-- kSwitch with delayed observation.
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

-- Spatial parallel composition of a signal function collection.
parB :: Functor col => col (SF a b) -> SF a (col b)
parB = par broadcast


-- Parallel switch (dynamic collection of signal functions spatially composed
-- in parallel).
pSwitchB :: Functor col =>
    col (SF a b) -> SF (a,col b) (Event c) -> (col (SF a b)->c-> SF a (col b))
    -> SF a (col b)
pSwitchB = pSwitch broadcast


dpSwitchB :: Functor col =>
    col (SF a b) -> SF (a,col b) (Event c) -> (col (SF a b)->c->SF a (col b))
    -> SF a (col b)
dpSwitchB = dpSwitch broadcast


rpSwitchB :: Functor col =>
    col (SF a b) -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)
rpSwitchB = rpSwitch broadcast


drpSwitchB :: Functor col =>
    col (SF a b) -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)
drpSwitchB = drpSwitch broadcast


------------------------------------------------------------------------------
-- Parallel composition and switching over collections with general routing
------------------------------------------------------------------------------

-- Spatial parallel composition of a signal function collection parameterized
-- on the routing function.
-- rf .........	Routing function: determines the input to each signal function
--		in the collection. IMPORTANT! The routing function MUST
--		preserve the structure of the signal function collection.
-- sfs0 .......	Signal function collection.
-- Returns the spatial parallel composition of the supplied signal functions.

par :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c)
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


-- Parallel switch parameterized on the routing function. This is the most
-- general switch from which all other (non-delayed) switches in principle
-- can be derived. The signal function collection is spatially composed in
-- parallel and run until the event signal function has an occurrence. Once
-- the switching event occurs, all signal function are "frozen" and their
-- continuations are passed to the continuation function, along with the
-- event value.
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
--
pSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c)
    -> SF (a, col c) (Event d)
    -> (col (SF b c) -> d -> SF a (col c))
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


-- Parallel switch with delayed observation parameterized on the routing
-- function.
--
-- !!! Could be optimized on the event source being SFArr, SFArrE, SFArrEE.
--
dpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c)
    -> SF (a, col c) (Event d)
    -> (col (SF b c) -> d -> SF a (col c))
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


------------------------------------------------------------------------------
-- Wave-form generation
------------------------------------------------------------------------------

-- Zero-order hold.
-- !!! Should be redone using SFSScan?
-- !!! Otherwise, we are missing an invarying case.
old_hold :: a -> SF (Event a) a
old_hold a_init = switch (constant a_init &&& identity)
                         ((NoEvent >--) . old_hold)

hold :: a -> SF (Event a) a
hold a_init = epPrim f () a_init
    where
        f _ a = ((), a, a)

-- !!!
-- !!! 2005-04-10: I DO NO LONGER THINK THIS IS CORRECT!
-- !!! CAN ONE POSSIBLY GET THE DESIRED STRICTNESS PROPERTIES
-- !!! ("DECOUPLING") this way???
-- !!! Also applies to the other "d" functions that were tentatively
-- !!! defined using only epPrim.
-- !!!
-- !!! 2005-06-13: Yes, indeed wrong! (But it's subtle, one has to
-- !!! make sure that the incoming event (and not just the payload
-- !!! of the event) is control dependent on  the output of "dHold"
-- !!! to observe it.
-- !!!
-- !!! 2005-06-09: But if iPre can be defined in terms of sscan,
-- !!! and ep + sscan = sscan, then things might work, and
-- !!! it might be possible to define dHold simply as hold >>> iPre
-- !!! without any performance penalty. 
-- Zero-order hold with delay.
-- Identity: dHold a0 = hold a0 >>> iPre a0).
dHold :: a -> SF (Event a) a
dHold a0 = hold a0 >>> iPre a0
{-
-- THIS IS WRONG! SEE ABOVE.
dHold a_init = epPrim f a_init a_init
    where
        f a' a = (a, a', a)
-}

-- Tracks input signal when available, holds last value when disappears.
-- !!! DANGER!!! Event used inside arr! Probably OK because arr will not be
-- !!! optimized to arrE. But still. Maybe rewrite this using, say, scan?
-- !!! or switch? Switching (in hold) for every input sample does not
-- !!! seem like such a great idea anyway.
trackAndHold :: a -> SF (Maybe a) a
trackAndHold a_init = arr (maybe NoEvent Event) >>> hold a_init


------------------------------------------------------------------------------
-- Accumulators
------------------------------------------------------------------------------

old_accum :: a -> SF (Event (a -> a)) (Event a)
old_accum = accumBy (flip ($))

accum :: a -> SF (Event (a -> a)) (Event a)
accum a_init = epPrim f a_init NoEvent
    where
        f a g = (a', Event a', NoEvent)
            where
                a' = g a


accumHold :: a -> SF (Event (a -> a)) a
accumHold a_init = epPrim f a_init a_init
    where
        f a g = (a', a', a')
            where
                a' = g a

dAccumHold :: a -> SF (Event (a -> a)) a
dAccumHold a_init = accumHold a_init >>> iPre a_init
{-
-- WRONG!
-- epPrim DOES and MUST patternmatch
-- on the input at every time step.
-- Test case to check for this added!
dAccumHold a_init = epPrim f a_init a_init
    where
        f a g = (a', a, a')
            where
                a' = g a
-}


old_accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
old_accumBy f b_init = switch (never &&& identity) $ \a -> abAux (f b_init a)
    where
        abAux b = switch (now b &&& notYet) $ \a -> abAux (f b a)

accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
accumBy g b_init = epPrim f b_init NoEvent
    where
        f b a = (b', Event b', NoEvent)
            where
                b' = g b a

accumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
accumHoldBy g b_init = epPrim f b_init b_init
    where
        f b a = (b', b', b')
            where
                b' = g b a

-- !!! This cannot be right since epPrim DOES and MUST patternmatch
-- !!! on the input at every time step.
-- !!! Add a test case to check for this!

dAccumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
dAccumHoldBy f a_init = accumHoldBy f a_init >>> iPre a_init
{-
-- WRONG!
-- epPrim DOES and MUST patternmatch
-- on the input at every time step.
-- Test case to check for this added!
dAccumHoldBy g b_init = epPrim f b_init b_init
    where
        f b a = (b', b, b')
            where
                b' = g b a
-}


{- Untested:

accumBy f b = switch (never &&& identity) $ \a ->
              let b' = f b a in NoEvent >-- Event b' --> accumBy f b'

But no real improvement in clarity anyway.

-}

-- accumBy f b = accumFilter (\b -> a -> let b' = f b a in (b', Event b')) b

{-
-- Identity: accumBy f = accumFilter (\b a -> let b' = f b a in (b',Just b'))
accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
accumBy f b_init = SF {sfTF = tf0}
    where
        tf0 NoEvent    = (abAux b_init, NoEvent) 
        tf0 (Event a0) = let b' = f b_init a0
		         in (abAux b', Event b')

        abAux b = SF' {sfTF' = tf}
	    where
		tf _ NoEvent   = (abAux b, NoEvent)
		tf _ (Event a) = let b' = f b a
			         in (abAux b', Event b')
-}

{-
accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
accumFilter f c_init = SF {sfTF = tf0}
    where
        tf0 NoEvent    = (afAux c_init, NoEvent) 
        tf0 (Event a0) = case f c_init a0 of
		             (c', Nothing) -> (afAux c', NoEvent)
			     (c', Just b0) -> (afAux c', Event b0)

        afAux c = SF' {sfTF' = tf}
	    where
		tf _ NoEvent   = (afAux c, NoEvent)
		tf _ (Event a) = case f c a of
			             (c', Nothing) -> (afAux c', NoEvent)
				     (c', Just b)  -> (afAux c', Event b)
-}


old_accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
old_accumFilter f c_init = switch (never &&& identity) $ \a -> afAux (f c_init a)
    where
        afAux (c, Nothing) = switch (never &&& notYet) $ \a -> afAux (f c a)
        afAux (c, Just b)  = switch (now b &&& notYet) $ \a -> afAux (f c a)

accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
accumFilter g c_init = epPrim f c_init NoEvent
    where
        f c a = case g c a of
                    (c', Nothing) -> (c', NoEvent, NoEvent)
                    (c', Just b)  -> (c', Event b, NoEvent)


------------------------------------------------------------------------------
-- Delays
------------------------------------------------------------------------------

-- Uninitialized delay operator.
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

-- Initialized delay operator.
old_iPre :: a -> SF a a
old_iPre = (--> old_pre)



-- !!! Redefined using SFSScan
-- !!! About 20% slower than old_pre on its own.
pre :: SF a a
pre = sscanPrim f uninit uninit
    where
        f c a = Just (a, c)
        uninit = usrErr "AFRP" "pre" "Uninitialized pre operator."


-- Initialized delay operator.
iPre :: a -> SF a a
iPre = (--> pre)


------------------------------------------------------------------------------
-- Timed delays
------------------------------------------------------------------------------


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
-- Integration and differentiation
------------------------------------------------------------------------------

-- Integration using the rectangle rule.
{-# INLINE integral #-}
integral :: VectorSpace a s => SF a a
integral = SF {sfTF = tf0}
    where
        igrl0  = zeroVector

	tf0 a0 = (integralAux igrl0 a0, igrl0)

	integralAux igrl a_prev = SF' tf -- True
	    where
	        tf dt a = (integralAux igrl' a, igrl')
		    where
		       igrl' = igrl ^+^ realToFrac dt *^ a_prev


-- "immediate" integration (using the function's value at the current time)
imIntegral :: VectorSpace a s => a -> SF a a
imIntegral = ((\ _ a' dt v -> v ^+^ realToFrac dt *^ a') `iterFrom`)

iterFrom :: (a -> a -> DTime -> b -> b) -> b -> SF a b
f `iterFrom` b = SF (iterAux b) where
  -- iterAux b a = (SF' (\ dt a' -> iterAux (f a a' dt b) a') True, b)
  iterAux b a = (SF' (\ dt a' -> iterAux (f a a' dt b) a'), b)


-- This is extremely crude. Use at your own risk.
derivative :: VectorSpace a s => SF a a
derivative = SF {sfTF = tf0}
    where
	tf0 a0 = (derivativeAux a0, zeroVector)

	derivativeAux a_prev = SF' tf -- True
	    where
	        tf dt a = (derivativeAux a, (a ^-^ a_prev) ^/ realToFrac dt)


------------------------------------------------------------------------------
-- Loops with guaranteed well-defined feedback
------------------------------------------------------------------------------

loopPre :: c -> SF (a,c) (b,c) -> SF a b
loopPre c_init sf = loop (second (iPre c_init) >>> sf)



loopIntegral :: VectorSpace c s => SF (a,c) (b,c) -> SF a b
loopIntegral sf = loop (second integral >>> sf)


------------------------------------------------------------------------------
-- Noise (i.e. random signal generators) and stochastic processes
------------------------------------------------------------------------------

-- Noise (random signal) with default range for type in question;
-- based on "randoms".
noise :: (RandomGen g, Random b) => g -> SF a b
noise g0 = streamToSF (randoms g0)


-- Noise (random signal) with specified range; based on "randomRs".
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


-- Stochastic event source with events occurring on average once every t_avg
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

    tf0 _ = (occAux ((randoms g) :: [Time]), NoEvent)

    occAux [] = undefined
    occAux (r:rs) = SF' tf -- True
        where
        tf dt _ = let p = 1 - exp (-(dt/t_avg)) -- Probability for at least one event.
                  in (occAux rs, if r < p then Event x else NoEvent)
                  


------------------------------------------------------------------------------
-- Reactimation
------------------------------------------------------------------------------

-- Reactimation of a signal function.
-- init .......	IO action for initialization. Will only be invoked once,
--		at (logical) time 0, before first call to "sense".
--		Expected to return the value of input at time 0.
-- sense ......	IO action for sensing of system input.
--	arg. #1 .......	True: action may block, waiting for an OS event.
--			False: action must not block.
--	res. #1 .......	Time interval since previous invocation of the sensing
--			action (or, the first time round, the init action),
--			returned. The interval must be _strictly_ greater
--			than 0. Thus even a non-blocking invocation must
--			ensure that time progresses.
--	res. #2 .......	Nothing: input is unchanged w.r.t. the previously
--			returned input sample.
--			Just i: the input is currently i.
--			It is OK to always return "Just", even if input is
--			unchanged.
-- actuate ....	IO action for outputting the system output.
--	arg. #1 .......	True: output may have changed from previous output
--			sample.
--			False: output is definitely unchanged from previous
--			output sample.
--			It is OK to ignore argument #1 and assume that the
--			the output has always changed.
--	arg. #2 .......	Current output sample.
--	result .......	Termination flag. Once True, reactimate will exit
--			the reactimation loop and return to its caller.
-- sf .........	Signal function to reactimate.

reactimate :: IO a
	      -> (Bool -> IO (DTime, Maybe a))
	      -> (Bool -> b -> IO Bool)
              -> SF a b
	      -> IO ()
reactimate init sense actuate (SF {sfTF = tf0}) =
    do
        a0 <- init
        let (sf, b0) = tf0 a0
        loop sf a0 b0
    where
        loop sf a b = do
	    done <- actuate True b
            unless (a `seq` b `seq` done) $ do
	        (dt, ma') <- sense False
		let a' = maybe a id ma'
                    (sf', b') = (sfTF' sf) dt a'
		loop sf' a' b'


-- An API for animating a signal function when some other library
-- needs to own the top-level control flow:

-- reactimate's state, maintained across samples:
data ReactState a b = ReactState {
    rsActuate :: ReactHandle a b -> Bool -> b -> IO Bool,
    rsSF :: SF' a b,
    rsA :: a,
    rsB :: b
  }	      

type ReactHandle a b = IORef (ReactState a b)

-- initialize top-level reaction handle
reactInit :: IO a -- init
             -> (ReactHandle a b -> Bool -> b -> IO Bool) -- actuate
             -> SF a b
             -> IO (ReactHandle a b)
reactInit init actuate (SF {sfTF = tf0}) = 
  do a0 <- init
     let (sf,b0) = tf0 a0
     -- TODO: really need to fix this interface, since right now we
     -- just ignore termination at time 0:
     r <- newIORef (ReactState {rsActuate = actuate, rsSF = sf,
				rsA = a0, rsB = b0 })
     done <- actuate r True b0
     return r

-- process a single input sample:
react :: ReactHandle a b
      -> (DTime,Maybe a)
      -> IO Bool
react rh (dt,ma') = 
  do rs@(ReactState {rsActuate = actuate,
	             rsSF = sf,
		     rsA = a,
		     rsB = b }) <- readIORef rh
     let a' = maybe a id ma'
         (sf',b') = (sfTF' sf) dt a'
     writeIORef rh (rs {rsSF = sf',rsA = a',rsB = b'})
     done <- actuate rh True b'
     return done     


------------------------------------------------------------------------------
-- Embedding
------------------------------------------------------------------------------

-- New embed interface. We will probably have to revisit this. To run an
-- embedded signal function while retaining full control (e.g. start and
-- stop at will), one would probably need a continuation based interface
-- (as well as a continuation based underlying implementation).
--
-- E.g. here are interesting alternative (or maybe complementary)
-- signatures:
--
--    sample :: SF a b -> SF (Event a) (Event b)
--    sample' :: SF a b -> SF (Event (DTime, a)) (Event b)
--
-- Maybe it should be called "subSample", since that's the only thing
-- that can be achieved. At least does not have the problem with missing
-- events when supersampling.
--
-- subSampleSynch :: SF a b -> SF (Event a) (Event b)
-- Time progresses at the same rate in the embedded system.
-- But it is only sampled on the events.
-- E.g.
-- repeatedly 0.1 () >>> subSampleSynch sf >>> hold
--
-- subSample :: DTime -> SF a b -> SF (Event a) (Event b)
-- Time advanced by dt for each event, not synchronized with the outer clock.

embed :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
embed sf0 (a0, dtas) = b0 : loop a0 sf dtas
    where
	(sf, b0) = (sfTF sf0) a0

        loop _ _ [] = []
	loop a_prev sf ((dt, ma) : dtas) =
	    b : (a `seq` b `seq` (loop a sf' dtas))
	    where
		a        = maybe a_prev id ma
	        (sf', b) = (sfTF' sf) dt a


-- Synchronous embedding. The embedded signal function is run on the supplied
-- input and time stream at a given (but variable) ratio >= 0 to the outer
-- time flow. When the ratio is 0, the embedded signal function is paused.
--
-- What about running an embedded signal function at a fixed (guaranteed)
-- sampling frequency? E.g. super sampling if the outer sampling is slower,
-- subsampling otherwise. AS WELL as at a given ratio to the outer one.
--
-- Ah, but that's more or less what embedSync does.
-- So just simplify the interface. But maybe it should also be possible
-- to feed in input from the enclosing system.

-- !!! Should "dropped frames" be forced to avoid space leaks?
-- !!! It's kind of hard to se why, but "frame dropping" was a problem
-- !!! in the old robot simulator. Try to find an example!

embedSynch :: SF a b -> (a, [(DTime, Maybe a)]) -> SF Double b
embedSynch sf0 (a0, dtas) = SF {sfTF = tf0}
    where
        tts       = scanl (\t (dt, _) -> t + dt) 0 dtas
	bbs@(b:_) = embed sf0 (a0, dtas)

	tf0 _ = (esAux 0 (zip tts bbs), b)

	esAux _       []    = intErr "AFRP" "embedSynch" "Empty list!"
        -- Invarying below since esAux [] is an error.
	esAux tp_prev tbtbs = SF' tf -- True
	    where
		tf dt r | r < 0     = usrErr "AFRP" "embedSynch"
					     "Negative ratio."
			| otherwise = let tp = tp_prev + dt * r
					  (b, tbtbs') = advance tp tbtbs
				      in
					  (esAux tp tbtbs', b)

		-- Advance the time stamped stream to the perceived time tp.
		-- Under the assumption that the perceived time never goes
		-- backwards (non-negative ratio), advance maintains the
		-- invariant that the perceived time is always >= the first
		-- time stamp.
        advance _  tbtbs@[(_, b)] = (b, tbtbs)
        advance tp tbtbtbs@((_, b) : tbtbs@((t', _) : _))
		    | tp <  t' = (b, tbtbtbs)
		    | t' <= tp = advance tp tbtbs
        advance _ _ = undefined

deltaEncode :: Eq a => DTime -> [a] -> (a, [(DTime, Maybe a)])
deltaEncode _  []        = usrErr "AFRP" "deltaEncode" "Empty input list."
deltaEncode dt aas@(_:_) = deltaEncodeBy (==) dt aas


deltaEncodeBy :: (a -> a -> Bool) -> DTime -> [a] -> (a, [(DTime, Maybe a)])
deltaEncodeBy _  _  []      = usrErr "AFRP" "deltaEncodeBy" "Empty input list."
deltaEncodeBy eq dt (a0:as) = (a0, zip (repeat dt) (debAux a0 as))
    where
	debAux _      []                     = []
	debAux a_prev (a:as) | a `eq` a_prev = Nothing : debAux a as
                             | otherwise     = Just a  : debAux a as 

-- Embedding and missing events.
-- Suppose a subsystem is super sampled. Then some of the output
-- samples will have to be dropped. If we are unlycky, the dropped
-- samples could be occurring events that we'd rather not miss.
-- This is a real problem.
-- Similarly, when feeding input into a super-sampled system,
-- we may need to extrapolate the input, assuming that it is
-- constant. But if (part of) the input is an occurring event, we'd
-- rather not duplicate that!!!
-- This suggests that:
--    * output samples should be merged through a user-supplied merge
--      function.
--    * input samples should be extrapolated if necessary through a
--      user-supplied extrapolation function.
--
-- Possible signature:
--
-- resample :: Time -> (c -> [a]) -> SF a b -> ([b] -> d) -> SF c d
--
-- But what do we do if the inner system runs more slowly than the
-- outer one? Then we need to extrapolate the output from the
-- inner system, and we have the same problem with events AGAIN!
