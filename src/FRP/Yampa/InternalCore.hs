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

module FRP.Yampa.InternalCore (
    module Control.Arrow,
    -- SF is an instance of Arrow and ArrowLoop. Method instances:
    -- arr      :: (a -> b) -> SF a b
    -- (>>>)    :: SF a b -> SF b c -> SF a c
    -- (<<<)    :: SF b c -> SF a b -> SF a c
    -- first    :: SF a b -> SF (a,c) (b,c)
    -- second   :: SF a b -> SF (c,a) (c,b)
    -- (***)    :: SF a b -> SF a' b' -> SF (a,a') (b,b')
    -- (&&&)    :: SF a b -> SF a b' -> SF a (b,b')
    -- returnA  :: SF a a
    -- loop     :: SF (a,c) (b,c) -> SF a b

    -- * Basic definitions
    -- ** Time
    Time,       -- [s] Both for time w.r.t. some reference and intervals.
    DTime,      -- [s] Sampling interval, always > 0.

    -- ** Signal Functions
    SF(..),             -- Signal Function.

    -- ** Future Signal Function
    SF'(..),            -- Signal Function.
    sfTF',
    sfId,
    sfConst,
    sfArrG,

    -- *** Scanning
    sfSScan,

    Transition,

    -- ** Function descriptions
    FunDesc(..),
    fdFun,

    -- ** Lifting
    arrPrim,
    arrEPrim, -- For optimization
    epPrim

) where

import Control.Arrow
#if __GLASGOW_HASKELL__ >= 610
import qualified Control.Category (Category(..))
#endif

import FRP.Yampa.Diagnostics
import FRP.Yampa.Miscellany (dup)
import FRP.Yampa.Event

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


-- | Time is used both for time intervals (duration), and time w.r.t. some
-- agreed reference point in time.

--  Conceptually, Time = R, i.e. time can be 0 -- or even negative.
type Time = Double      -- [s]


-- | DTime is the time type for lengths of sample intervals. Conceptually,
-- DTime = R+ = { x in R | x > 0 }. Don't assume Time and DTime have the
-- same representation.
type DTime = Double     -- [s]

-- Representation of signal function in initial state.
-- (Naming: "TF" stands for Transition Function.)

-- | Signal function that transforms a signal carrying values of some type 'a'
-- into a signal carrying values of some type 'b'. You can think of it as
-- (Signal a -> Signal b). A signal is, conceptually, a
-- function from 'Time' to value.
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

-- sfNever :: SF' a (Event b)
-- sfNever = sfConst NoEvent

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
    FDI :: FunDesc a a                                  -- Identity function
    FDC :: b -> FunDesc a b                             -- Constant function
    FDE :: (Event a -> b) -> b -> FunDesc (Event a) b   -- Event-processing fun
    FDG :: (a -> b) -> FunDesc a b                      -- General function

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



------------------------------------------------------------------------------
-- Arrow instance and implementation
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 610
instance Control.Category.Category SF where
     (.) = flip compPrim
     id = SF $ \x -> (sfId,x)
#endif

instance ArrowChoice SF where
    left sf = SF $ \a ->
                     -- NOTE: there might be a problem with choice here.
                     -- Do the delta times accumulate for the unused branch?
                     -- Recommendation by Olivier Charles: take a look
                     -- at Settable Signals paper, it discusses which
                     -- option would be best.
                     case a of
                       Left x  -> let (sf', b') = sfTF sf x
                                  in (futureArrowLeft sf', Left b')
                       Right x -> let sf' = SF' $ \_ -> sfTF sf
                                  in (futureArrowLeft sf', Right x)
       where futureArrowLeft fSF = SF' $ \dt a ->
                case a of
                  Left x  -> let (sf', b') = sfTF' fSF dt x
                             in (futureArrowLeft sf', Left b')
                  Right x -> (futureArrowLeft fSF, Right x)
          

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

-- * Lifting.

-- | Lifts a pure function into a signal function (applied pointwise).
{-# NOINLINE arrPrim #-}
arrPrim :: (a -> b) -> SF a b
arrPrim f = SF {sfTF = \a -> (sfArrG f, f a)}

-- | Lifts a pure function into a signal function applied to events
--   (applied pointwise).
{-# RULES "arrPrim/arrEPrim" arrPrim = arrEPrim #-}
arrEPrim :: (Event a -> b) -> SF (Event a) b
arrEPrim f = SF {sfTF = \a -> (sfArrE f (f NoEvent), f a)}


-- * Composition.
-- The definition exploits the following identities:
--     sf         >>> identity   = sf                           -- New
--     identity   >>> sf         = sf                           -- New
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
cpXX sf1@(SFEP{}) (SFCpAXA _ (FDE f21 f21ne) sf22 fd23) =
    cpXX (cpXE sf1 f21 f21ne) (cpXA sf22 fd23)
-- f21 will (hopefully) be invoked less frequently if merged with the
-- event processor.
cpXX sf1@(SFEP{}) (SFCpAXA _ (FDG f21) sf22 fd23) =
    cpXX (cpXG sf1 f21) (cpXA sf22 fd23)
-- Only functions whose domain is known to be Event can be merged
-- from the left with event processors.
cpXX (SFCpAXA _ fd11 sf12 (FDE f13 f13ne)) sf2@(SFEP{}) =
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
        cpAXAAux fd1 _ fd3 _ sf2@(SFSScan {}) =
            cpAX fd1 (cpXA sf2 fd3)
        cpAXAAux fd1 _ fd3 _ sf2@(SFEP {}) =
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
                    case f1 (Event a) of
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


-- * Widening.
-- The definition exploits the following identities:
--     first identity     = identity                            -- New
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
fpAux (SFArr _ FDI)       = sfId                        -- New
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
spAux (SFArr _ FDI)       = sfId                        -- New
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


-- * Parallel composition.
-- The definition exploits the following identities (that hold for SF):
--     identity   *** identity   = identity             -- New
--     sf         *** identity   = first sf             -- New
--     identity   *** sf         = second sf            -- New
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
        psXX (SFArr _ FDI)       sf2                 = spAux sf2        -- New
        psXX (SFArr _ (FDC b))   sf2                 = psCX b sf2
        psXX (SFArr _ fd1)       sf2                 = psAX (fdFun fd1) sf2
        psXX sf1                 (SFArr _ FDI)       = fpAux sf1        -- New
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


-- * ArrowLoop instance and implementation

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

-- * Scanning
--
sfSScan :: (c -> a -> Maybe (c, b)) -> c -> b -> SF' a b
sfSScan f c b = sf
    where
        sf = SFSScan tf f c b
        tf _ a = case f c a of
                     Nothing       -> (sf, b)
                     Just (c', b') -> (sfSScan f c' b', b')

-- Vim modeline
-- vim:set tabstop=8 expandtab:
