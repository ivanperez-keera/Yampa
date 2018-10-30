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
-- Yampa can be used to program purely functional reactive systems in a
-- deterministic, elegant and efficient way.
--
-- Yampa has been used to write professional Haskell cross-platform games for
-- iOS, Android, desktop and web.   There is a library for testing Yampa
-- applications that allows you to use Temporal Logic and QuickCheck to test
-- your games. You can also use a time-travel debugger to connect to your
-- application running live and debug it step by step.
--
-- You can find examples, screenshots, tutorials and documentation here:
--
-- <https://github.com/ivanperez-keera/Yampa>
--
-- <https://github.com/ivanperez-keera/Yampa/tree/master/examples>
--
-- <https://wiki.haskell.org/Yampa>
--
-- A Yampa network is structured as a Signal Function: a pure transformation
-- from a time-varying input to that produces a time-varying output. The Yampa
-- language provides signal function primitives, as well as SF combinators.
-- Primitives and combinators guarantee that SFs are well-formed and efficient.
--
-- For example, a game could take the changing mouse position (continuous-time
-- signal) and mouse clicks (discrete-time signal), combine them as part of
-- some game logic, and produce an animation with sound (continuously changing
-- picture).
--
-- To create a Yampa system, you need to think about three things:
--
-- * How to obtain the input signals coming into your system. This typically
-- requires polling some input device or consuming a queue of input events.
--
-- * How to consume the output signals produced by your system. This typically
-- requires taking output samples or chunks and rendering them or playing them.
--
-- * How to transform the input signal into the output signal. This requires
-- thinking about the transformation applied as time progresses towards the
-- future, possinly switching from one transformation to another as the program
-- evolves.
--
-- The first two aspects lie outside Yampa, and they determine the backends
-- that your system uses. Yampa is backend-agnostic, and you can connect it to
-- SDL, SDL2, OpenGL, Gloss, Diagrams, HTML5 Canvas. In addition, you can use
-- it with any input device you want, and it has been used with Nintendo
-- wiimotes, Microsoft Kinects and LeapMotions.
--
-- The last aspect is what defines Yampa as a language. You define a pure
-- Signal Function (@SF@) using primitives and combinators. You can find a
-- series of primitive SFs in "FRP.Yampa.Basic". For example, the function
-- 'constant' allows you to ignore the input signal and produce a constant
-- output, the function 'arr' allows you to apply a pure function to every
-- input value at every time, ignoring previous history. Signal Functions can
-- transform signals taking their history into account. For example, the
-- function 'integral' integrates the input signal.
--
-- Signals can change in continuous or in discrete time (known as 'Event's).
-- Events represent values that may or may not occur (and would probably occur
-- rarely). It is often used for incoming network messages, mouse clicks, etc.
-- Events are used as values carried by signals.  The module "FRP.Yampa.Event"
-- allows you to manipulate events, the module "FRP.Yampa.EventS" deals with
-- event signal functions, and the "FRP.Yampa.Hybrid" allows you to go from a
-- continuous-time domain to a discrete domain, and vice-versa.
--
-- The execution of this signal transformer with specific input can be
-- accomplished by means of two functions: 'reactimate' (which needs an
-- initialization action, an input sensing action and an actuation/consumer
-- action and executes until explicitly stopped), and 'react' (which executes
-- only one cycle). You can also use the function 'embed' to try your signal
-- functions with lists of input samples in GHCi.
--
-- For a simple example of an SDL application that creates a moving picture
-- around the mouse position, see:
--
-- https://github.com/ivanperez-keera/Yampa/blob/develop/examples/yampa-game/MainCircleMouse.hs
--
-- Table of Contents
--
-- Main Yampa modules:
--
-- * "FRP.Yampa"            -- This exports all FRP-related functions
--
-- * "FRP.Yampa.Task"
--
-- Different FRP aspects:
--
-- * "FRP.Yampa.Basic"        -- Primitive SFs
--
-- * "FRP.Yampa.Conditional"  -- Apply one SF or another depending on a condition
--
-- * "FRP.Yampa.Delays"       -- Delay a signal
--
-- * "FRP.Yampa.Event"        -- Event combinators
--
-- * "FRP.Yampa.EventS"       -- Event Signal Functions
--
-- * "FRP.Yampa.Hybrid"       -- Continuous-time to Discrete-time combinators
--
-- * "FRP.Yampa.Integration"  -- Integration and derivation and sums
--
-- * "FRP.Yampa.Loop"         -- Feedback loops
--
-- * "FRP.Yampa.Random"       -- Random signals
--
-- * "FRP.Yampa.Scan"         -- Scanning or folding a signal
--
-- * "FRP.Yampa.Switches"     -- Dynamically changing an SF based on the value of a signal
--
-- * "FRP.Yampa.Time"         -- Signals that represent time
--
-- Execution:
--
-- * "FRP.Yampa.Simulation" -- Reactimation/evaluation

-- ToDo:
--
-- - Specialize def. of repeatedly. Could have an impact on invaders.
--
-- - New defs for accs using SFAcc
--
-- - Make sure opt worked: e.g.
--
-- - >     repeatedly >>> count >>> arr (fmap sqr)
--
-- - Introduce SFAccHld.
--
-- - See if possible to unify AccHld wity Acc??? They are so close.
--
-- - Introduce SScan. BUT KEEP IN MIND: Most if not all opts would
-- - have been possible without GADTs???
--
-- - Look into pairs. At least pairing of SScan ought to be interesting.
--
-- - Would be nice if we could get rid of first & second with impunity
-- - thanks to Id optimizations. That's a clear win, with or without
-- - an explicit pair combinator.
--
-- - delayEventCat is a bit complicated ...
--
--
-- Random ideas:
--
-- - What if one used rules to optimize
--   - (arr :: SF a ()) to (constant ())
--   - (arr :: SF a a) to identity
--   But inspection of invader source code seem to indicate that
--   these are not very common cases at all.
--
-- - It would be nice if it was possible to come up with opt. rules
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
-- - The transition function would still be optimized in (pretty much)
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
-- - The ONLY change was changing the constructor from SF' to SFComp and
--   adding sf1 and sf2 to the constructor app.!
--
-- - An optimized case:
--     cpAuxC1 b sf1 sf2               = SFComp tf sf1 sf2
--   So cpAuxC1 gets an extra arg, and we change the constructor.
--   But how to exploit without writing 1000s of rules???
--   Maybe define predicates on SFComp to see if the first or second
--   sf are "interesting", and if so, make "reassociate" and make a
--   recursive call? E.g. we're in the arr case, and the first sf is another
--   arr, so we'd like to combine the two.
--
-- - It would also be intersting, then, to know when to STOP playing this
--   game, due to the overhead involved.
--
-- - Why don't we have a "SWITCH" constructor that indicates that the
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

    -- * Basic definitions
    Time,       -- [s] Both for time w.r.t. some reference and intervals.
    DTime,      -- [s] Sampling interval, always > 0.
    SF,         -- Signal Function.
    Event(..),  -- Events; conceptually similar to Maybe (but abstract).

    -- Temporary!
    --    SF(..), sfTF',

    -- Main instances
    -- SF is an instance of Arrow and ArrowLoop. Method instances:
    -- arr     :: (a -> b) -> SF a b
    -- (>>>)   :: SF a b -> SF b c -> SF a c
    -- (<<<)   :: SF b c -> SF a b -> SF a c
    -- first   :: SF a b -> SF (a,c) (b,c)
    -- second  :: SF a b -> SF (c,a) (c,b)
    -- (***)   :: SF a b -> SF a' b' -> SF (a,a') (b,b')
    -- (&&&)   :: SF a b -> SF a b' -> SF a (b,b')
    -- returnA :: SF a a
    -- loop    :: SF (a,c) (b,c) -> SF a b

    -- Event is an instance of Functor, Eq, and Ord. Some method instances:
    -- fmap    :: (a -> b) -> Event a -> Event b
    -- (==)     :: Event a -> Event a -> Bool
    -- (<=)    :: Event a -> Event a -> Bool

    -- ** Lifting
    arrPrim, arrEPrim, -- For optimization

    -- * Signal functions

    -- ** Basic signal functions
    identity,             -- :: SF a a
    constant,             -- :: b -> SF a b
    localTime,            -- :: SF a Time
    time,                 -- :: SF a Time,    Other name for localTime.

    -- ** Initialization
    (-->),                -- :: b -> SF a b -> SF a b,        infixr 0
    (-:>),                -- :: b -> SF a b -> SF a b,        infixr 0
    (>--),                -- :: a -> SF a b -> SF a b,        infixr 0
    (-=>),                -- :: (b -> b) -> SF a b -> SF a b      infixr 0
    (>=-),                -- :: (a -> a) -> SF a b -> SF a b      infixr 0
    initially,            -- :: a -> SF a a

    -- ** Simple, stateful signal processing
    sscan,                -- :: (b -> a -> b) -> b -> SF a b
    sscanPrim,            -- :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b

    -- * Events
    -- ** Basic event sources
    never,                -- :: SF a (Event b)
    now,                  -- :: b -> SF a (Event b)
    after,                -- :: Time -> b -> SF a (Event b)
    repeatedly,           -- :: Time -> b -> SF a (Event b)
    afterEach,            -- :: [(Time,b)] -> SF a (Event b)
    afterEachCat,         -- :: [(Time,b)] -> SF a (Event [b])
    delayEvent,           -- :: Time -> SF (Event a) (Event a)
    delayEventCat,        -- :: Time -> SF (Event a) (Event [a])
    edge,                 -- :: SF Bool (Event ())
    iEdge,                -- :: Bool -> SF Bool (Event ())
    edgeTag,              -- :: a -> SF Bool (Event a)
    edgeJust,             -- :: SF (Maybe a) (Event a)
    edgeBy,               -- :: (a -> a -> Maybe b) -> a -> SF a (Event b)
    maybeToEvent,         -- :: Maybe a -> Event a

    -- ** Stateful event suppression
    notYet,               -- :: SF (Event a) (Event a)
    once,                 -- :: SF (Event a) (Event a)
    takeEvents,           -- :: Int -> SF (Event a) (Event a)
    dropEvents,           -- :: Int -> SF (Event a) (Event a)

    -- ** Pointwise functions on events
    noEvent,              -- :: Event a
    noEventFst,           -- :: (Event a, b) -> (Event c, b)
    noEventSnd,           -- :: (a, Event b) -> (a, Event c)
    event,                -- :: a -> (b -> a) -> Event b -> a
    fromEvent,            -- :: Event a -> a
    isEvent,              -- :: Event a -> Bool
    isNoEvent,            -- :: Event a -> Bool
    tag,                  -- :: Event a -> b -> Event b,        infixl 8
    tagWith,              -- :: b -> Event a -> Event b,
    attach,               -- :: Event a -> b -> Event (a, b),    infixl 8
    lMerge,               -- :: Event a -> Event a -> Event a,    infixl 6
    rMerge,               -- :: Event a -> Event a -> Event a,    infixl 6
    merge,                -- :: Event a -> Event a -> Event a,    infixl 6
    mergeBy,              -- :: (a -> a -> a) -> Event a -> Event a -> Event a
    mapMerge,             -- :: (a -> c) -> (b -> c) -> (a -> b -> c)
                          --    -> Event a -> Event b -> Event c
    mergeEvents,          -- :: [Event a] -> Event a
    catEvents,            -- :: [Event a] -> Event [a]
    joinE,                -- :: Event a -> Event b -> Event (a,b),infixl 7
    splitE,               -- :: Event (a,b) -> (Event a, Event b)
    filterE,              -- :: (a -> Bool) -> Event a -> Event a
    mapFilterE,           -- :: (a -> Maybe b) -> Event a -> Event b
    gate,                 -- :: Event a -> Bool -> Event a,    infixl 8

    -- * Switching
    -- ** Basic switchers
    switch,  dSwitch,     -- :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
    rSwitch, drSwitch,    -- :: SF a b -> SF (a,Event (SF a b)) b
    kSwitch, dkSwitch,    -- :: SF a b
                          --    -> SF (a,b) (Event c)
                          --    -> (SF a b -> c -> SF a b)
                          --    -> SF a b

    -- ** Parallel composition and switching
    -- *** Parallel composition and switching over collections with broadcasting
    parB,                 -- :: Functor col => col (SF a b) -> SF a (col b)
    pSwitchB,dpSwitchB,   -- :: Functor col =>
                          --        col (SF a b)
                          --      -> SF (a, col b) (Event c)
                          --      -> (col (SF a b) -> c -> SF a (col b))
                          --      -> SF a (col b)
    rpSwitchB,drpSwitchB, -- :: Functor col =>
                          --        col (SF a b)
                          --      -> SF (a, Event (col (SF a b)->col (SF a b)))
                          --            (col b)

    -- *** Parallel composition and switching over collections with general routing
    par,                  -- Functor col =>
                          --     (forall sf . (a -> col sf -> col (b, sf)))
                          --     -> col (SF b c)
                          --     -> SF a (col c)
    pSwitch, dpSwitch,    -- pSwitch :: Functor col =>
                          --     (forall sf . (a -> col sf -> col (b, sf)))
                          --     -> col (SF b c)
                          --     -> SF (a, col c) (Event d)
                          --     -> (col (SF b c) -> d -> SF a (col c))
                          --     -> SF a (col c)
    rpSwitch,drpSwitch,   -- Functor col =>
                          --    (forall sf . (a -> col sf -> col (b, sf)))
                          --    -> col (SF b c)
                          --    -> SF (a, Event (col (SF b c) -> col (SF b c)))
                          --          (col c)
                          --

    -- * Discrete to continuous-time signal functions
    -- ** Wave-form generation
    hold,                 -- :: a -> SF (Event a) a
    dHold,                -- :: a -> SF (Event a) a
    trackAndHold,         -- :: a -> SF (Maybe a) a

    -- ** Accumulators
    accum,                -- :: a -> SF (Event (a -> a)) (Event a)
    accumHold,            -- :: a -> SF (Event (a -> a)) a
    dAccumHold,           -- :: a -> SF (Event (a -> a)) a
    accumBy,              -- :: (b -> a -> b) -> b -> SF (Event a) (Event b)
    accumHoldBy,          -- :: (b -> a -> b) -> b -> SF (Event a) b
    dAccumHoldBy,         -- :: (b -> a -> b) -> b -> SF (Event a) b
    accumFilter,          -- :: (c -> a -> (c, Maybe b)) -> c
                          --    -> SF (Event a) (Event b)

    -- * Delays
    -- ** Basic delays
    pre,                  -- :: SF a a
    iPre,                 -- :: a -> SF a a

    -- ** Timed delays
    delay,                -- :: Time -> a -> SF a a

    -- ** Variable delay
    pause,                -- :: b -> SF a b -> SF a Bool -> SF a b

    -- * State keeping combinators

    -- ** Loops with guaranteed well-defined feedback
    loopPre,              -- :: c -> SF (a,c) (b,c) -> SF a b
    loopIntegral,         -- :: VectorSpace c s => SF (a,c) (b,c) -> SF a b

    -- ** Integration and differentiation
    integral,             -- :: VectorSpace a s => SF a a
    imIntegral,           -- :: VectorSpace a s => a -> SF a a
    impulseIntegral,      -- :: VectorSpace a k => SF (a, Event a) a
    count,                -- :: Integral b => SF (Event a) (Event b)
    derivative,           -- :: VectorSpace a s => SF a a        -- Crude!


    -- Temporarily hidden, but will eventually be made public.
    iterFrom,          -- :: (a -> a -> DTime -> b -> b) -> b -> SF a b

    -- * Noise (random signal) sources and stochastic event sources
    noise,                -- :: noise :: (RandomGen g, Random b) =>
                          --              g -> SF a b
    noiseR,               -- :: noise :: (RandomGen g, Random b) =>
                          --             (b,b) -> g -> SF a b
    occasionally,         -- :: RandomGen g => g -> Time -> b -> SF a (Event b)

    -- * Execution/simulation
    -- ** Reactimation
    reactimate,           -- :: IO a
                          --    -> (Bool -> IO (DTime, Maybe a))
                          --    -> (Bool -> b -> IO Bool)
                          --    -> SF a b
                          --    -> IO ()
    ReactHandle,
    reactInit,            --    IO a -- init
                          --    -> (ReactHandle a b -> Bool -> b -> IO Bool) -- actuate
                          --    -> SF a b
                          --    -> IO (ReactHandle a b)
                          -- process a single input sample:
    react,                --    ReactHandle a b
                          --    -> (DTime,Maybe a)
                          --    -> IO Bool

    -- ** Embedding
                          --  (tentative: will be revisited)
    embed,                -- :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
    embedSynch,           -- :: SF a b -> (a, [(DTime, Maybe a)]) -> SF Double b
    deltaEncode,          -- :: Eq a => DTime -> [a] -> (a, [(DTime, Maybe a)])
    deltaEncodeBy,        -- :: (a -> a -> Bool) -> DTime -> [a]
                          --    -> (a, [(DTime, Maybe a)])

    FutureSF,
    evalAtZero,
    evalAt,
    evalFuture,

    -- * Auxiliary definitions
    --   Reverse function composition and arrow plumbing aids
    ( # ),                -- :: (a -> b) -> (b -> c) -> (a -> c),    infixl 9
    dup,                  -- :: a -> (a,a)

) where

import Control.Arrow

import FRP.Yampa.InternalCore
import FRP.Yampa.Basic
import FRP.Yampa.Conditional
import FRP.Yampa.Delays
import FRP.Yampa.Event
import FRP.Yampa.EventS
import FRP.Yampa.Hybrid
import FRP.Yampa.Integration
import FRP.Yampa.Loop
import FRP.Yampa.Miscellany (( # ), dup)
import FRP.Yampa.Random
import FRP.Yampa.Scan
import FRP.Yampa.Simulation
import FRP.Yampa.Switches
import FRP.Yampa.Time
import FRP.Yampa.VectorSpace

-- Vim modeline
-- vim:set tabstop=8 expandtab:
