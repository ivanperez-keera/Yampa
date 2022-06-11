{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}
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

module FRP.Yampa.InternalCore
    ( module Control.Arrow

      -- * Basic definitions
      -- ** Time
    , Time
    , DTime

      -- ** Signal Functions
    , SF(..)

      -- ** Future Signal Function
    , SF'(..)
    , Transition
    , sfTF'
    , sfId
    , sfConst
    , sfArrG

      -- *** Scanning
    , sfSScan

      -- ** Function descriptions
    , FunDesc(..)
    , fdFun

      -- ** Lifting
    , arrPrim
    , arrEPrim
    , epPrim
    )
  where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..))
#endif

import Control.Arrow

#if __GLASGOW_HASKELL__ >= 610
import qualified Control.Category (Category(..))
#endif

import FRP.Yampa.Diagnostics
import FRP.Yampa.Event

-- * Basic type definitions with associated utilities

-- | Time is used both for time intervals (duration), and time w.r.t. some
-- agreed reference point in time.

--  Conceptually, Time = R, i.e. time can be 0 -- or even negative.
type Time = Double      -- [s]

-- | DTime is the time type for lengths of sample intervals. Conceptually,
-- DTime = R+ = { x in R | x > 0 }. Don't assume Time and DTime have the
-- same representation.
type DTime = Double     -- [s]

-- | Signal function that transforms a signal carrying values of some type 'a'
-- into a signal carrying values of some type 'b'. You can think of it as
-- (Signal a -> Signal b). A signal is, conceptually, a
-- function from 'Time' to value.
data SF a b = SF {sfTF :: a -> Transition a b}

-- | Signal function in "running" state.
--
--   It can also be seen as a Future Signal Function, meaning,
--   an SF that, given a time delta or a time in the future, it will
--   be an SF.
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
  SF' :: !(DTime -> a -> Transition a b) -> SF' a b

-- | A transition is a pair of the next state (in the form of a future signal
-- function) and the output at the present time step.

type Transition a b = (SF' a b, b)

-- | Obtain the function that defines a running SF.
sfTF' :: SF' a b -> (DTime -> a -> Transition a b)
sfTF' (SFArr tf _)       = tf
sfTF' (SFSScan tf _ _ _) = tf
sfTF' (SFEP tf _ _ _)    = tf
sfTF' (SFCpAXA tf _ _ _) = tf
sfTF' (SF' tf)           = tf

-- | Constructor for a lifted structured function.
sfArr :: FunDesc a b -> SF' a b
sfArr FDI         = sfId
sfArr (FDC b)     = sfConst b
sfArr (FDE f fne) = sfArrE f fne
sfArr (FDG f)     = sfArrG f

-- | SF constructor for the identity function.
sfId :: SF' a a
sfId = sf
  where
    sf = SFArr (\_ a -> (sf, a)) FDI

-- | SF constructor for the constant function.
sfConst :: b -> SF' a b
sfConst b = sf
  where
    sf = SFArr (\_ _ -> (sf, b)) (FDC b)

-- Assumption: fne = f NoEvent
sfArrE :: (Event a -> b) -> b -> SF' (Event a) b
sfArrE f fne = sf
  where
    sf  = SFArr (\_ ea -> (sf, case ea of NoEvent -> fne ; _ -> f ea))
                (FDE f fne)

-- | SF constructor for a general function.
sfArrG :: (a -> b) -> SF' a b
sfArrG f = sf
  where
    sf = SFArr (\_ a -> (sf, f a)) (FDG f)

-- | Versatile zero-order hold SF' with folding.
--
--   This function returns an SF that, if there is an input, runs it
--   through the given function and returns part of its output and, if not,
--   returns the last known output.
--
--   The auxiliary function returns the value of the current output and
--   the future held output, thus making it possible to have to distinct
--   outputs for the present and the future.
epPrim :: (c -> a -> (c, b, b)) -> c -> b -> SF (Event a) b
epPrim f c bne = SF {sfTF = tf0}
  where
    tf0 NoEvent   = (sfEP f c bne, bne)
    tf0 (Event a) = let (c', b, bne') = f c a
                    in (sfEP f c' bne', b)

-- | Constructor for a zero-order hold SF' with folding.
--
--   This function returns a running SF that, if there is an input, runs it
--   through the given function and returns part of its output and, if not,
--   returns the last known output.
--
--   The auxiliary function returns the value of the current output and
--   the future held output, thus making it possible to have to distinct
--   outputs for the present and the future.
sfEP :: (c -> a -> (c, b, b)) -> c -> b -> SF' (Event a) b
sfEP f c bne = sf
  where
    sf = SFEP (\_ ea -> case ea of
                          NoEvent -> (sf, bne)
                          Event a -> let (c', b, bne') = f c a
                                     in (sfEP f c' bne', b))
              f
              c
              bne

-- | Structured function definition.
--
--   This type represents functions with a bit more structure, providing
--   specific constructors for the identity, constant and event-based
--   functions, helping optimise arrow combinators for special cases.
data FunDesc a b where
  FDI :: FunDesc a a                                  -- Identity function
  FDC :: b -> FunDesc a b                             -- Constant function
  FDE :: (Event a -> b) -> b -> FunDesc (Event a) b   -- Event-processing fun
  FDG :: (a -> b) -> FunDesc a b                      -- General function

-- | Turns a function into a structured function.
fdFun :: FunDesc a b -> (a -> b)
fdFun FDI       = id
fdFun (FDC b)   = const b
fdFun (FDE f _) = f
fdFun (FDG f)   = f

-- | Composition for structured functions.
fdComp :: FunDesc a b -> FunDesc b c -> FunDesc a c
fdComp FDI           fd2     = fd2
fdComp fd1           FDI     = fd1
fdComp (FDC b)       fd2     = FDC ((fdFun fd2) b)
fdComp _             (FDC c) = FDC c
fdComp (FDE f1 f1ne) fd2     = FDE (f2 . f1) (f2 f1ne)
  where
    f2 = fdFun fd2
fdComp (FDG f1) (FDE f2 f2ne) = FDG f
  where
    f a = case f1 a of
            NoEvent -> f2ne
            f1a     -> f2 f1a
fdComp (FDG f1) fd2 = FDG (fdFun fd2 . f1)

-- | Parallel application of structured functions.
fdPar :: FunDesc a b -> FunDesc c d -> FunDesc (a,c) (b,d)
fdPar FDI     FDI     = FDI
fdPar FDI     (FDC d) = FDG (\(~(a, _)) -> (a, d))
fdPar FDI     fd2     = FDG (\(~(a, c)) -> (a, (fdFun fd2) c))
fdPar (FDC b) FDI     = FDG (\(~(_, c)) -> (b, c))
fdPar (FDC b) (FDC d) = FDC (b, d)
fdPar (FDC b) fd2     = FDG (\(~(_, c)) -> (b, (fdFun fd2) c))
fdPar fd1     fd2     = FDG (\(~(a, c)) -> ((fdFun fd1) a, (fdFun fd2) c))

-- | Parallel application with broadcasting for structured functions.
fdFanOut :: FunDesc a b -> FunDesc a c -> FunDesc a (b,c)
fdFanOut FDI     FDI     = FDG (\a -> (a, a))
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

-- | Verifies that the first argument is NoEvent. Returns the value of the
-- second argument that is the case. Raises an error otherwise.
-- Used to check that functions on events do not map NoEvent to Event
-- wherever that assumption is exploited.
vfyNoEv :: Event a -> b -> b
vfyNoEv NoEvent b = b
vfyNoEv _       _  =
  usrErr
    "Yampa"
    "vfyNoEv"
    "Assertion failed: Functions on events must not map NoEvent to Event."

-- * Arrow instance and implementation

#if __GLASGOW_HASKELL__ >= 610
-- | Composition and identity for SFs.
instance Control.Category.Category SF where
  (.) = flip compPrim
  id = SF $ \x -> (sfId,x)
#endif

-- | Choice of which SF to run based on the value of a signal.
instance ArrowChoice SF where
  -- (+++) :: forall b c b' c'
  --       .  SF b c -> SF d e -> SF (Either b d) (Either c e)
  sfL +++ sfR = SF $ \a ->
      case a of
        Left b  -> let (sf', c) = sfTF sfL b
                   in (chooseL sf' sfR, Left c)
        Right d -> let (sf', e) = sfTF sfR d
                   in (chooseR sfL sf', Right e)

    where

      -- (+++) for an initialized SF and an SF
      --
      -- chooseL :: SF' b c -> SF d e -> SF' (Either b d) (Either c e)
      chooseL sfCL sfR = SF' $ \dt a ->
        case a of
          Left b  -> let (sf', c) = sfTF' sfCL dt b
                     in (chooseL sf' sfR, Left c)
          Right d -> let (sf', e) = sfTF sfR d
                     in (choose sfCL sf', Right e)

      -- (+++) for an SF and an initialized SF
      --
      -- chooseR :: SF b c -> SF' d e -> SF' (Either b d) (Either c e)
      chooseR sfL sfCR = SF' $ \dt a ->
        case a of
          Left b  -> let (sf', c) = sfTF sfL b
                     in (choose sf' sfCR, Left c)
          Right d -> let (sf', e) = sfTF' sfCR dt d
                     in (chooseR sfL sf', Right e)

      -- (+++) for initialized SFs
      --
      -- choose :: SF' b c -> SF' d e -> SF' (Either b d) (Either c e)
      choose sfCL sfCR = SF' $ \dt a ->
        case a of
          Left b  -> let (sf', c) = sfTF' sfCL dt b
                     in (choose sf' sfCR, Left c)
          Right d -> let (sf', e) = sfTF' sfCR dt d
                     in (choose sfCL sf', Right e)

-- | Signal Functions as Arrows. See "The Yampa Arcade", by Courtney, Nilsson
--   and Peterson.
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

-- | Functor instance for applied SFs.
instance Functor (SF a) where
  fmap f = (>>> arr f)

-- | Applicative Functor instance (allows classic-frp style signals and
-- composition using applicative style).
instance Applicative (SF a) where
  pure x = arr (const x)
  f <*>  x  = (f &&& x) >>> arr (uncurry ($))

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

-- | SF Composition
--
-- The definition exploits the following identities:
--     sf         >>> identity   = sf                           -- New
--     identity   >>> sf         = sf                           -- New
--     sf         >>> constant c = constant c
--     constant c >>> arr f      = constant (f c)
--     arr f      >>> arr g      = arr (g . f)
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
cpXX (SFSScan _ f1 s1 b) (SFSScan _ f2 s2 c) =
    sfSScan f (s1, b, s2, c) c
  where
    f (s1, b, s2, c) a =
        let (u, s1',  b') = case f1 s1 a of
                              Nothing       -> (True, s1, b)
                              Just (s1',b') -> (False,  s1', b')
        in case f2 s2 b' of
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
            Event b -> let (s2', c, cne') = f2 s2 b
                       in Just ((s1, eb, s2', cne'), c)
        Just (s1', eb') ->
          case eb' of
            NoEvent -> Just ((s1', eb', s2, cne), cne)
            Event b -> let (s2', c, cne') = f2 s2 b
                       in Just ((s1', eb', s2', cne'), c)

cpXX (SFEP _ f1 s1 bne) (SFSScan _ f2 s2 c) =
    sfSScan f (s1, bne, s2, c) c
  where
    f (s1, bne, s2, c) ea =
      let (u, s1', b', bne') = case ea of
                                 NoEvent -> (True, s1, bne, bne)
                                 Event a -> let (s1', b, bne') = f1 s1 a
                                            in (False, s1', b, bne')
      in case f2 s2 b' of
           Nothing | u         -> Nothing
                   | otherwise -> Just (seq s1' (s1', bne', s2, c), c)
           Just (s2', c') -> Just (seq s1' (s1', bne', s2', c'), c')
cpXX (SFEP _ f1 s1 bne) (SFEP _ f2 s2 cne) =
    sfEP f (s1, s2, cne) (vfyNoEv bne cne)
  where
    -- The function "f" is invoked whenever an event is to be processed. It
    -- then computes the output, the new state, and the new NoEvent output.
    -- However, when sequencing event processors, the ones in the latter
    -- part of the chain may not get invoked since previous ones may decide
    -- not to "fire". But a "new" NoEvent output still has to be produced,
    -- i.e. the old one retained. Since it cannot be computed by invoking
    -- the last event-processing function in the chain, it has to be
    -- remembered. Since the composite event-processing function remains
    -- constant/unchanged, the NoEvent output has to be part of the state.
    -- An alternative would be to make the event-processing function take
    -- an extra argument. But that is likely to make the simple case more
    -- expensive. See note at sfEP.
    f (s1, s2, cne) a =
      case f1 s1 a of
        (s1', NoEvent, NoEvent) -> ((s1', s2, cne), cne, cne)
        (s1', Event b, NoEvent) ->
          let (s2', c, cne') = f2 s2 b in ((s1', s2', cne'), c, cne')
        _ -> usrErr "Yampa" "cpXX" $
               "Assertion failed: Functions on events must not map "
               ++ "NoEvent to Event."
cpXX sf1@(SFEP{}) (SFCpAXA _ (FDE f21 f21ne) sf22 fd23) =
  cpXX (cpXE sf1 f21 f21ne) (cpXA sf22 fd23)
cpXX sf1@(SFEP{}) (SFCpAXA _ (FDG f21) sf22 fd23) =
  cpXX (cpXG sf1 f21) (cpXA sf22 fd23)
cpXX (SFCpAXA _ fd11 sf12 (FDE f13 f13ne)) sf2@(SFEP{}) =
  cpXX (cpAX fd11 sf12) (cpEX f13 f13ne sf2)
cpXX (SFCpAXA _ fd11 sf12 fd13) (SFCpAXA _ fd21 sf22 fd23) =
  -- Termination: The first argument to cpXX is no larger than
  -- the current first argument, and the second is smaller.
  cpAXA fd11 (cpXX (cpXA sf12 (fdComp fd13 fd21)) sf22) fd23
cpXX sf1 sf2 = SF' tf --  False
  where
    tf dt a = (cpXX sf1' sf2', c)
      where
        (sf1', b) = (sfTF' sf1) dt a
        (sf2', c) = (sfTF' sf2) dt b

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

      where
        tf dt a = (cpAXAAux fd1 f1 fd3 f3 sf2', f3 c)
          where
            (sf2', c) = (sfTF' sf2) dt (f1 a)

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

-- The remaining signal function, if it is SF', later could turn into something
-- else, like SFId.
cpCX :: b -> SF' b c -> SF' a c
cpCX b (SFArr _ fd2) = sfConst ((fdFun fd2) b)
cpCX b (SFSScan _ f s c) = sfSScan (\s _ -> f s b) s c
cpCX b (SFEP _ _ _ cne) = sfConst (vfyNoEv b cne)
cpCX b (SFCpAXA _ fd21 sf22 fd23) =
  cpCXA ((fdFun fd21) b) sf22 fd23
cpCX b sf2 = SFCpAXA tf (FDC b) sf2 FDI
  where
    tf dt _ = (cpCX b sf2', c)
      where
        (sf2', c) = (sfTF' sf2) dt b

cpCXA :: b -> SF' b c -> FunDesc c d -> SF' a d
cpCXA b sf2 FDI     = cpCX b sf2
cpCXA _ _   (FDC c) = sfConst c
cpCXA b sf2 fd3     = cpCXAAux (FDC b) b fd3 (fdFun fd3) sf2
  where
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
      where
        tf dt _ = (cpCXAAux fd1 b fd3 f3 sf2', f3 c)
          where
            (sf2', c) = (sfTF' sf2) dt b

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
      where
        tf dt a = (cpGXAux fd1 f1 sf2', c)
          where
            (sf2', c) = (sfTF' sf2) dt (f1 a)

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
      where
        tf dt a = (cpXGAux fd2 f2 sf1', f2 b)
          where
            (sf1', b) = (sfTF' sf1) dt a

cpEX :: (Event a -> b) -> b -> SF' b c -> SF' (Event a) c
cpEX f1 f1ne sf2 = cpEXAux (FDE f1 f1ne) f1 f1ne sf2
  where
    cpEXAux :: FunDesc (Event a) b -> (Event a -> b) -> b
               -> SF' b c -> SF' (Event a) c
    cpEXAux fd1 _ _ (SFArr _ fd2) = sfArr (fdComp fd1 fd2)
    cpEXAux _ f1 _   (SFSScan _ f s c) = sfSScan (\s a -> f s (f1 a)) s c
    -- We must not capture cne in the f closure since cne can change!  See cpXX
    -- the SFEP/SFEP case for a similar situation. However, FDE represent a
    -- state-less signal function, so *its* NoEvent value never changes. Hence
    -- we only need to verify that it is NoEvent once.
    cpEXAux _ f1 f1ne (SFEP _ f2 s cne) =
        sfEP f (s, cne) (vfyNoEv f1ne cne)
      where
        f scne@(s, cne) a =
          case f1 (Event a) of
            NoEvent -> (scne, cne, cne)
            Event b -> let (s', c, cne') = f2 s b in ((s', cne'), c, cne')
    cpEXAux fd1 _ _ (SFCpAXA _ fd21 sf22 fd23) =
      cpAXA (fdComp fd1 fd21) sf22 fd23
    -- The rationale for the following is that the case analysis is typically
    -- not going to be more expensive than applying the function and possibly a
    -- bit cheaper. Thus if events are sparse, we might win, and if not, we
    -- don't loose to much.
    cpEXAux fd1 f1 f1ne sf2 = SFCpAXA tf fd1 sf2 FDI
      where
        tf dt ea = (cpEXAux fd1 f1 f1ne sf2', c)
          where
            (sf2', c) =
              case ea of
                NoEvent -> (sfTF' sf2) dt f1ne
                _       -> (sfTF' sf2) dt (f1 ea)

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
            _ -> usrErr "Yampa" "cpXEAux" $
                   "Assertion failed: Functions on events must not "
                   ++ "map NoEvent to Event."
    cpXEAux fd2 _ _ (SFCpAXA _ fd11 sf12 fd13) =
      cpAXA fd11 sf12 (fdComp fd13 fd2)
    cpXEAux fd2 f2 f2ne sf1 = SFCpAXA tf FDI sf1 fd2
      where
        tf dt a = ( cpXEAux fd2 f2 f2ne sf1'
                  , case eb of NoEvent -> f2ne; _ -> f2 eb
                  )
          where
            (sf1', eb) = (sfTF' sf1) dt a

-- * Widening.

-- | Widening
--
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

fpAux :: SF' a b -> SF' (a,c) (b,c)
fpAux (SFArr _ FDI)       = sfId                        -- New
fpAux (SFArr _ (FDC b))   = sfArrG (\(~(_, c)) -> (b, c))
fpAux (SFArr _ fd1)       = sfArrG (\(~(a, c)) -> ((fdFun fd1) a, c))
fpAux sf1 = SF' tf
  where
    tf dt ~(a, c) = (fpAux sf1', (b, c))
      where
        (sf1', b) = (sfTF' sf1) dt a

-- Mirror image of first.
secondPrim :: SF a b -> SF (c,a) (c,b)
secondPrim (SF {sfTF = tf10}) = SF {sfTF = tf0}
  where
    tf0 ~(c0, a0) = (spAux sf1, (c0, b0))
      where
        (sf1, b0) = tf10 a0

spAux :: SF' a b -> SF' (c,a) (c,b)
spAux (SFArr _ FDI)       = sfId                        -- New
spAux (SFArr _ (FDC b))   = sfArrG (\(~(c, _)) -> (c, b))
spAux (SFArr _ fd1)       = sfArrG (\(~(c, a)) -> (c, (fdFun fd1) a))
spAux sf1 = SF' tf
  where
    tf dt ~(c, a) = (spAux sf1', (c, b))
      where
        (sf1', b) = (sfTF' sf1) dt a

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
    psXX sf1 sf2 = SF' tf
      where
        tf dt ~(a, c) = (psXX sf1' sf2', (b, d))
          where
            (sf1', b) = (sfTF' sf1) dt a
            (sf2', d) = (sfTF' sf2) dt c

    psCX :: b -> SF' c d -> SF' (a,c) (b,d)
    psCX b (SFArr _ fd2)       = sfArr (fdPar (FDC b) fd2)
    psCX b sf2                 = SF' tf
      where
        tf dt ~(_, c) = (psCX b sf2', (b, d))
          where
            (sf2', d) = (sfTF' sf2) dt c

    psXC :: SF' a b -> d -> SF' (a,c) (b,d)
    psXC (SFArr _ fd1)       d = sfArr (fdPar fd1 (FDC d))
    psXC sf1                 d = SF' tf
      where
        tf dt ~(a, _) = (psXC sf1' d, (b, d))
          where
            (sf1', b) = (sfTF' sf1) dt a

    psAX :: (a -> b) -> SF' c d -> SF' (a,c) (b,d)
    psAX f1 (SFArr _ fd2)       = sfArr (fdPar (FDG f1) fd2)
    psAX f1 sf2                 = SF' tf
      where
        tf dt ~(a, c) = (psAX f1 sf2', (f1 a, d))
          where
            (sf2', d) = (sfTF' sf2) dt c

    psXA :: SF' a b -> (c -> d) -> SF' (a,c) (b,d)
    psXA (SFArr _ fd1)       f2 = sfArr (fdPar fd1 (FDG f2))
    psXA sf1                 f2 = SF' tf
      where
        tf dt ~(a, c) = (psXA sf1' f2, (b, f2 c))
          where
            (sf1', b) = (sfTF' sf1) dt a

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
    pfoXX sf1 sf2 = SF' tf
      where
        tf dt a = (pfoXX sf1' sf2', (b, c))
          where
            (sf1', b) = (sfTF' sf1) dt a
            (sf2', c) = (sfTF' sf2) dt a

    pfoIX :: SF' a c -> SF' a (a ,c)
    pfoIX (SFArr _ fd2) = sfArr (fdFanOut FDI fd2)
    pfoIX sf2 = SF' tf
      where
        tf dt a = (pfoIX sf2', (a, c))
          where
            (sf2', c) = (sfTF' sf2) dt a

    pfoXI :: SF' a b -> SF' a (b ,a)
    pfoXI (SFArr _ fd1) = sfArr (fdFanOut fd1 FDI)
    pfoXI sf1 = SF' tf
      where
        tf dt a = (pfoXI sf1', (b, a))
          where
            (sf1', b) = (sfTF' sf1) dt a

    pfoCX :: b -> SF' a c -> SF' a (b ,c)
    pfoCX b (SFArr _ fd2) = sfArr (fdFanOut (FDC b) fd2)
    pfoCX b sf2 = SF' tf
      where
        tf dt a = (pfoCX b sf2', (b, c))
          where
            (sf2', c) = (sfTF' sf2) dt a

    pfoXC :: SF' a b -> c -> SF' a (b ,c)
    pfoXC (SFArr _ fd1) c = sfArr (fdFanOut fd1 (FDC c))
    pfoXC sf1 c = SF' tf
      where
        tf dt a = (pfoXC sf1' c, (b, c))
          where
            (sf1', b) = (sfTF' sf1) dt a

    pfoAX :: (a -> b) -> SF' a c -> SF' a (b ,c)
    pfoAX f1 (SFArr _ fd2) = sfArr (fdFanOut (FDG f1) fd2)
    pfoAX f1 sf2 = SF' tf
      where
        tf dt a = (pfoAX f1 sf2', (f1 a, c))
          where
            (sf2', c) = (sfTF' sf2) dt a

    pfoXA :: SF' a b -> (a -> c) -> SF' a (b ,c)
    pfoXA (SFArr _ fd1) f2 = sfArr (fdFanOut fd1 (FDG f2))
    pfoXA sf1 f2 = SF' tf
      where
        tf dt a = (pfoXA sf1' f2, (b, f2 a))
          where
            (sf1', b) = (sfTF' sf1) dt a

-- * ArrowLoop instance and implementation

-- | Creates a feedback loop without delay.
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
      where
        tf dt a = (loopAux sf1', b)
          where
            (sf1', (b, c)) = (sfTF' sf1) dt (a, c)

-- * Scanning

-- | Constructor for a zero-order hold with folding.
--
--   This function returns a running SF that takes an input, runs it through a
--   function and, if there is an output, returns it, otherwise, returns the
--   previous value. Additionally, an accumulator or folded value is kept
--   internally.
sfSScan :: (c -> a -> Maybe (c, b)) -> c -> b -> SF' a b
sfSScan f c b = sf
  where
    sf = SFSScan tf f c b
    tf _ a = case f c a of
               Nothing       -> (sf, b)
               Just (c', b') -> (sfSScan f c' b', b')
