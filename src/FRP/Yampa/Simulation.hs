{-# LANGUAGE GADTs, Rank2Types, CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Simulation
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Execution/simulation of signal functions.
--
-- SFs can be executed in two ways: by running them, feeding input samples one
-- by one, obtained from a monadic environment (presumably, @IO@), or by
-- passing an input stream and calculating an output stream. The former is
-- called /reactimation/, and the latter is called /embedding/.
--
-- * Running:
-- Normally, to run an SF, you would use 'reactimate', providing input samples,
-- and consuming the output samples in the 'IO' monad. This function takes over
-- the program, implementing a "main loop". If you want more control over the
-- evaluation loop (for instance, if you are using Yampa in combination with a
-- backend that also implements some main loop), you may want to use the
-- lower-level API for reactimation ('ReactHandle', 'reactInit', 'react').
--
-- * Embedding:
-- You can use 'embed' for testing, to evaluate SFs in a terminal, and to embed
-- an SF inside a larger system. The helper functions 'deltaEncode' and
-- 'deltaEncodeBy' facilitate producing input /signals/ from plain lists of
-- input samples.
--
-- This module also includes debugging aids needed to execute signal functions
-- step by step, which are used by Yampa's testing facilities.
-----------------------------------------------------------------------------------------

module FRP.Yampa.Simulation (
   -- * Reactimation
    reactimate,         -- :: IO a
                        --    -> (Bool -> IO (DTime, Maybe a))
                        --    -> (Bool -> b -> IO Bool)
                        --    -> SF a b
                        --    -> IO ()
                        --
    -- ** Low-level reactimation interface
    ReactHandle,
    reactInit,          --    IO a -- init
                        --    -> (ReactHandle a b -> Bool -> b -> IO Bool) -- actuate
                        --    -> SF a b
                        --    -> IO (ReactHandle a b)
                        -- process a single input sample:
    react,              --    ReactHandle a b
                        --    -> (DTime,Maybe a)
                        --    -> IO Bool

    -- * Embedding
    embed,              -- :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
    embedSynch,         -- :: SF a b -> (a, [(DTime, Maybe a)]) -> SF Double b
    deltaEncode,        -- :: Eq a => DTime -> [a] -> (a, [(DTime, Maybe a)])
    deltaEncodeBy,      -- :: (a -> a -> Bool) -> DTime -> [a]
                        --    -> (a, [(DTime, Maybe a)])

    -- * Debugging / Step by step simulation

    FutureSF,
    evalAtZero,
    evalAt,
    evalFuture,


) where

import Control.Monad (unless)
import Data.IORef
import Data.Maybe (fromMaybe)

import FRP.Yampa.InternalCore (SF(..), SF'(..), sfTF', DTime)

import FRP.Yampa.Diagnostics

------------------------------------------------------------------------------
-- Reactimation
------------------------------------------------------------------------------

-- Reactimation of a signal function.
-- init ....... IO action for initialization. Will only be invoked once,
--              at (logical) time 0, before first call to "sense".
--              Expected to return the value of input at time 0.
-- sense ...... IO action for sensing of system input.
--      arg. #1 ....... True: action may block, waiting for an OS event.
--                      False: action must not block.
--      res. #1 ....... Time interval since previous invocation of the sensing
--                      action (or, the first time round, the init action),
--                      returned. The interval must be _strictly_ greater
--                      than 0. Thus even a non-blocking invocation must
--                      ensure that time progresses.
--      res. #2 ....... Nothing: input is unchanged w.r.t. the previously
--                      returned input sample.
--                      Just i: the input is currently i.
--                      It is OK to always return "Just", even if input is
--                      unchanged.
-- actuate .... IO action for outputting the system output.
--      arg. #1 ....... True: output may have changed from previous output
--                      sample.
--                      False: output is definitely unchanged from previous
--                      output sample.
--                      It is OK to ignore argument #1 and assume that the
--                      the output has always changed.
--      arg. #2 ....... Current output sample.
--      result .......  Termination flag. Once True, reactimate will exit
--                      the reactimation loop and return to its caller.
-- sf ......... Signal function to reactimate.

-- | Convenience function to run a signal function indefinitely, using
-- a IO actions to obtain new input and process the output.
--
-- This function first runs the initialization action, which provides the
-- initial input for the signal transformer at time 0.
--
-- Afterwards, an input sensing action is used to obtain new input (if any) and
-- the time since the last iteration. The argument to the input sensing function
-- indicates if it can block. If no new input is received, it is assumed to be
-- the same as in the last iteration.
--
-- After applying the signal function to the input, the actuation IO action
-- is executed. The first argument indicates if the output has changed, the second
-- gives the actual output). Actuation functions may choose to ignore the first
-- argument altogether. This action should return True if the reactimation
-- must stop, and False if it should continue.
--
-- Note that this becomes the program's /main loop/, which makes using this
-- function incompatible with GLUT, Gtk and other graphics libraries. It may also
-- impose a sizeable constraint in larger projects in which different subparts run
-- at different time steps. If you need to control the main
-- loop yourself for these or other reasons, use 'reactInit' and 'react'.

reactimate :: Monad m
           => m a                             -- ^ Initialization action
           -> (Bool -> m (DTime, Maybe a))    -- ^ Input sensing action
           -> (Bool -> b -> m Bool)           -- ^ Actuation (output processing) action
           -> SF a b                          -- ^ Signal function
           -> m ()
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
                let a' = fromMaybe a ma'
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

-- | A reference to reactimate's state, maintained across samples.
newtype ReactHandle a b = ReactHandle
  { reactHandle :: IORef (ReactState a b) }

-- | Initialize a top-level reaction handle.
reactInit :: IO a -- init
             -> (ReactHandle a b -> Bool -> b -> IO Bool) -- actuate
             -> SF a b
             -> IO (ReactHandle a b)
reactInit init actuate (SF {sfTF = tf0}) =
  do a0 <- init
     let (sf,b0) = tf0 a0
     -- TODO: really need to fix this interface, since right now we
     -- just ignore termination at time 0:
     r' <- newIORef (ReactState {rsActuate = actuate, rsSF = sf, rsA = a0, rsB = b0 })
     let r = ReactHandle r'
     _ <- actuate r True b0
     return r

-- | Process a single input sample.
react :: ReactHandle a b
      -> (DTime,Maybe a)
      -> IO Bool
react rh (dt,ma') =
  do rs@(ReactState {rsActuate = actuate, rsSF = sf, rsA = a, rsB = _b }) <- readIORef (reactHandle rh)
     let a' = fromMaybe a ma'
         (sf',b') = (sfTF' sf) dt a'
     writeIORef (reactHandle rh) (rs {rsSF = sf',rsA = a',rsB = b'})
     done <- actuate rh True b'
     return done


------------------------------------------------------------------------------
-- Embedding
------------------------------------------------------------------------------

-- New embed interface. We will probably have to revisit this. To run an
-- embedded signal function while retaining full control (e.g. start and
-- stop at will), one would probably need a continuation-based interface
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

-- | Given a signal function and a pair with an initial
-- input sample for the input signal, and a list of sampling
-- times, possibly with new input samples at those times,
-- it produces a list of output samples.
--
-- This is a simplified, purely-functional version of 'reactimate'.
embed :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
embed sf0 (a0, dtas) = b0 : loop a0 sf dtas
    where
        (sf, b0) = (sfTF sf0) a0

        loop _ _ [] = []
        loop a_prev sf ((dt, ma) : dtas) =
            b : (a `seq` b `seq` loop a sf' dtas)
            where
                a        = fromMaybe a_prev ma
                (sf', b) = (sfTF' sf) dt a


-- | Synchronous embedding. The embedded signal function is run on the supplied
-- input and time stream at a given (but variable) ratio >= 0 to the outer
-- time flow. When the ratio is 0, the embedded signal function is paused.

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

-- | Spaces a list of samples by a fixed time delta, avoiding
--   unnecessary samples when the input has not changed since
--   the last sample.
deltaEncode :: Eq a => DTime -> [a] -> (a, [(DTime, Maybe a)])
deltaEncode _  []        = usrErr "AFRP" "deltaEncode" "Empty input list."
deltaEncode dt aas@(_:_) = deltaEncodeBy (==) dt aas


-- | 'deltaEncode' parameterized by the equality test.
deltaEncodeBy :: (a -> a -> Bool) -> DTime -> [a] -> (a, [(DTime, Maybe a)])
deltaEncodeBy _  _  []      = usrErr "AFRP" "deltaEncodeBy" "Empty input list."
deltaEncodeBy eq dt (a0:as) = (a0, zip (repeat dt) (debAux a0 as))
    where
        debAux _      []                     = []
        debAux a_prev (a:as) | a `eq` a_prev = Nothing : debAux a as
                             | otherwise     = Just a  : debAux a as


-- * Debugging / Step by step simulation

-- | A wrapper around an initialized SF (continuation), needed for testing and
-- debugging purposes.
--
newtype FutureSF a b = FutureSF { unsafeSF :: SF' a b }


-- | Evaluate an SF, and return an output and an initialized SF.
--
--   /WARN/: Do not use this function for standard simulation. This function is
--   intended only for debugging/testing. Apart from being potentially slower
--   and consuming more memory, it also breaks the FRP abstraction by making
--   samples discrete and step based.
evalAtZero :: SF a b
           -> a                  -- ^ Input sample
           -> (b, FutureSF a b)  -- ^ Output x Continuation
evalAtZero (SF { sfTF = tf }) a = (b, FutureSF tf' )
  where (tf', b) = tf a


-- | Evaluate an initialized SF, and return an output and a continuation.
--
--   /WARN/: Do not use this function for standard simulation. This function is
--   intended only for debugging/testing. Apart from being potentially slower
--   and consuming more memory, it also breaks the FRP abstraction by making
--   samples discrete and step based.
evalAt :: FutureSF a b
       -> DTime -> a         -- ^ Input sample
       -> (b, FutureSF a b)  -- ^ Output x Continuation
evalAt (FutureSF { unsafeSF = tf }) dt a = (b, FutureSF tf')
  where (tf', b) = (sfTF' tf) dt a


-- | Given a signal function and time delta, it moves the signal function into
--   the future, returning a new uninitialized SF and the initial output.
--
--   While the input sample refers to the present, the time delta refers to the
--   future (or to the time between the current sample and the next sample).
--
--   /WARN/: Do not use this function for standard simulation. This function is
--   intended only for debugging/testing. Apart from being potentially slower
--   and consuming more memory, it also breaks the FRP abstraction by making
--   samples discrete and step based.
--
evalFuture :: SF a b -> a -> DTime -> (b, SF a b)
evalFuture sf a dt = (b, sf' dt)
  where (b, sf') = evalStep sf a


-- | Steps the signal function into the future one step. It returns the current
-- output, and a signal function that expects, apart from an input, a time
-- between samples.
evalStep :: SF a b -> a -> (b, DTime -> SF a b)
evalStep (SF sf) a = (b, \dt -> SF (sfTF' sf' dt))
  where (sf', b) = sf a

-- Vim modeline
-- vim:set tabstop=8 expandtab:
