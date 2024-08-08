-- |
-- Module      :  FRP.Yampa
-- Copyright   :  (c) Ivan Perez, 2014-2022
--                (c) George Giorgidze, 2007-2012
--                (c) Henrik Nilsson, 2005-2006
--                (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Domain-specific language embedded in Haskell for programming deterministic
-- hybrid (mixed discrete-time and continuous-time) systems. Yampa is based on
-- the concepts of Functional Reactive Programming (FRP).
--
-- Yampa has been used to write professional Haskell cross-platform games for
-- iOS, Android, desktop and web. There is a library for testing Yampa
-- applications that allows you to use Temporal Logic and QuickCheck to test
-- your games. You can also use a time-travel debugger to connect to your
-- application running live and debug it step by step.
--
-- __Documentation__
--
-- You can find many examples, tutorials and documentation here:
--
-- <https://github.com/ivanperez-keera/Yampa>
--
-- <https://github.com/ivanperez-keera/Yampa/tree/develop/yampa/examples>
--
-- <https://wiki.haskell.org/Yampa>
--
-- __Yampa at a glance__
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
-- /Signal and SF separation/
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
-- future, possibly switching from one transformation to another as the program
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
-- /Execution/
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
-- https://github.com/ivanperez-keera/Yampa/blob/develop/yampa/examples/yampa-game/MainCircleMouse.hs
--
-- /Hybrid systems/
--
-- Signals can change in continuous or in discrete time (known as 'Event's).
-- Events represent values that may or may not occur (and would probably occur
-- rarely). It is often used for incoming network messages, mouse clicks, etc.
-- Events are used as values carried by signals.  The module "FRP.Yampa.Event"
-- allows you to manipulate events, the module "FRP.Yampa.EventS" deals with
-- event signal functions, and the "FRP.Yampa.Hybrid" allows you to go from a
-- continuous-time domain to a discrete domain, and vice-versa.
--
-- /Vector Spaces/
--
-- Yampa uses vector spaces in time-aware primitives like 'integral'. However,
-- Yampa does not enforce the use of a particular vector space implementation,
-- meaning you could use 'integral' for example with other vector types like
-- V2, V1, etc. from the library linear. For an example, see
-- <https://gist.github.com/walseb/1e0a0ca98aaa9469ab5da04e24f482c2 this gist>.
--
--
-- __Library Overview__
--
-- * Main Yampa module
--
--     * "FRP.Yampa"              -- Exports all FRP-related functions.
--
-- * Different FRP aspects
--
--     * "FRP.Yampa.Basic"        -- Primitive SFs.
--
--     * "FRP.Yampa.Conditional"  -- Apply one SF or another depending on
--                                   a condition.
--
--     * "FRP.Yampa.Delays"       -- Delay a signal.
--
--     * "FRP.Yampa.Event"        -- Event combinators.
--
--     * "FRP.Yampa.EventS"       -- Event Signal Functions.
--
--     * "FRP.Yampa.Hybrid"       -- Continuous-time to Discrete-time
--                                   combinators.
--
--     * "FRP.Yampa.Integration"  -- Integration and derivation and sums.
--
--     * "FRP.Yampa.Loop"         -- Feedback loops.
--
--     * "FRP.Yampa.Random"       -- Random signals.
--
--     * "FRP.Yampa.Scan"         -- Scanning or folding a signal.
--
--     * "FRP.Yampa.Switches"     -- Dynamically changing an SF based on the
--                                   value of a signal.
--
--     * "FRP.Yampa.Task"         -- SFs that terminate and are followed by
--                                   other SFs.
--
--     * "FRP.Yampa.Time"         -- Signals that represent time.
--
-- * Execution
--
--     * "FRP.Yampa.Simulation" -- Reactimation/evaluation.
--
-- * Auxiliary modules
--
--     * "FRP.Yampa.Arrow" -- Arrow-generic functions.
module FRP.Yampa
    (
      -- * Basic definitions
      Time
    , DTime
    , SF
    , Event(..)

      -- ** Lifting
    , arrPrim, arrEPrim

      -- * Signal functions

      -- ** Basic signal functions
    , identity
    , constant
    , localTime
    , time

      -- ** Initialization
    , (-->)
    , (-:>)
    , (>--)
    , (-=>)
    , (>=-)
    , initially

      -- ** Simple, stateful signal processing
    , sscan
    , sscanPrim

      -- * Events
      -- ** Basic event sources
    , never
    , now
    , after
    , repeatedly
    , afterEach
    , afterEachCat
    , delayEvent
    , delayEventCat
    , edge
    , iEdge
    , edgeTag
    , edgeJust
    , edgeBy
    , maybeToEvent

      -- ** Stateful event suppression
    , notYet
    , once
    , takeEvents
    , dropEvents

      -- ** Pointwise functions on events
    , noEvent
    , noEventFst
    , noEventSnd
    , event
    , fromEvent
    , isEvent
    , isNoEvent
    , tag
    , tagWith
    , attach
    , lMerge
    , rMerge
    , merge
    , mergeBy
    , mapMerge
    , mergeEvents
    , catEvents
    , joinE
    , splitE
    , filterE
    , mapFilterE
    , gate

      -- * Switching
      -- ** Basic switchers
    , switch,  dSwitch
    , rSwitch, drSwitch
    , kSwitch, dkSwitch

      -- ** Parallel composition and switching
      -- *** Parallel composition and switching with broadcasting
    , parB
    , pSwitchB, dpSwitchB
    , rpSwitchB, drpSwitchB

      -- *** Parallel composition and switching with general routing
    , par
    , pSwitch,  dpSwitch
    , rpSwitch, drpSwitch

      -- * Discrete to continuous-time signal functions
      -- ** Wave-form generation
    , hold
    , dHold
    , trackAndHold

      -- ** Accumulators
    , accum
    , accumHold
    , dAccumHold
    , accumBy
    , accumHoldBy
    , dAccumHoldBy
    , accumFilter

      -- * Delays
      -- ** Basic delays
    , pre
    , iPre

      -- ** Timed delays
    , delay

      -- ** Variable delay
    , pause

      -- * State keeping combinators

      -- ** Loops with guaranteed well-defined feedback
    , loopPre
    , loopIntegral

      -- ** Integration and differentiation
    , integral
    , imIntegral
    , trapezoidIntegral
    , impulseIntegral
    , count
    , derivative

      -- Temporarily hidden, but will eventually be made public.
    , iterFrom

      -- * Noise (random signal) sources and stochastic event sources
    , noise
    , noiseR
    , occasionally

    , RandomGen(..)
    , Random(..)

      -- * Execution/simulation
      -- ** Reactimation
    , reactimate
    , ReactHandle
    , reactInit
    , react

      -- ** Embedding
    , embed
    , embedSynch
    , deltaEncode
    , deltaEncodeBy

    , FutureSF
    , evalAtZero
    , evalAt
    , evalFuture

      -- * Auxiliary definitions
      --   Reverse function composition and arrow plumbing aids
    , dup

      -- * Re-exported module, classes, and types
    , module Control.Arrow
    , module Data.VectorSpace
    )
  where

-- External modules
import Control.Arrow
import Data.VectorSpace

-- Internal modules
import FRP.Yampa.Arrow (dup)
import FRP.Yampa.Basic
import FRP.Yampa.Conditional
import FRP.Yampa.Delays
import FRP.Yampa.Event
import FRP.Yampa.EventS
import FRP.Yampa.Hybrid
import FRP.Yampa.Integration
import FRP.Yampa.InternalCore
import FRP.Yampa.Loop
import FRP.Yampa.Random
import FRP.Yampa.Scan
import FRP.Yampa.Simulation
import FRP.Yampa.Switches
import FRP.Yampa.Time
