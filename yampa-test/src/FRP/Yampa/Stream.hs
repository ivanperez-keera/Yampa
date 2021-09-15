{-# LANGUAGE MultiWayIf #-}
-- | Streams and stream manipulation API.
--
-- The evaluation of Yampa SFs, especially for testing purposes, needs the
-- generation of suitable input streams.
--
-- While some streams can be generated randomly using QuickCheck, it is
-- sometimes useful to be able to preprend or adapt an input stream. It is also
-- useful to debug programs when you have recorded input streams using Haskell
-- Titan.
--
-- This module defines types for input streams, as well as an API to create,
-- examine and combine streams. It also provides evaluation functions that are
-- needed to apply an SF to a stream and obtain an output stream and a
-- continuation SF.
module FRP.Yampa.Stream where

import FRP.Yampa (DTime, SF, FutureSF, evalAtZero, evalAt)

-- * Types

-- | A stream of samples, with their sampling times.
type SignalSampleStream a = (a, FutureSampleStream a)

-- | A stream of future samples, with their sampling times. The difference
-- between 'SignalSampleStream' and 'FutureSampleStream' is that all elements
-- in the latter have a non-zero time delta.
type FutureSampleStream a = [(DTime, a)]

-- * Creation

-- | Group a series of samples with a series of time deltas.
--
--   The first sample will have no delta. Unused samples and deltas will be
--   dropped.
groupDeltas :: [a] -> [DTime] -> SignalSampleStream a
groupDeltas (x:xs) ds = (x, zip ds xs)
groupDeltas xs     ds = error $ "groupDeltas: called me with lists with lengths" ++ show (length xs) ++ " and " ++ show (length ds)

-- * Examination

-- | Turn a stream with sampling times into a list of values.
samples :: SignalSampleStream a -> [a]
samples (a, as) = a : map snd as

-- | Return the first sample in a sample stream.
firstSample :: SignalSampleStream a -> a
firstSample = head . samples

-- | Return the last sample in a sample stream.
lastSample :: SignalSampleStream a -> a
lastSample = last . samples

-- * Manipulation

-- | Merge two streams, using an auxilary function to merge samples that fall
-- at the exact same sampling time.
sMerge :: (a -> a -> a) -> SignalSampleStream a -> SignalSampleStream a -> SignalSampleStream a
sMerge f (x1, xs1) (x2, xs2) = (f x1 x2, sMergeTail f xs1 xs2)
  where
    sMergeTail :: (a -> a -> a) -> FutureSampleStream a -> FutureSampleStream a -> FutureSampleStream a
    sMergeTail f []              xs2             = xs2
    sMergeTail f xs1             []              = xs1
    sMergeTail f ((dt1, x1):xs1) ((dt2, x2):xs2)
      | dt1 == dt2 = (dt1, f x1 x2) : sMergeTail f xs1 xs2
      | dt1 <  dt2 = (dt1, x1) : sMergeTail f xs1 ((dt2-dt1, x2):xs2)
      | otherwise  = (dt2, x2) : sMergeTail f ((dt1-dt2, x1):xs1) xs2

-- | Concatenate two sample streams, separating them by a given time delta.
sConcat :: SignalSampleStream a -> DTime -> SignalSampleStream a -> SignalSampleStream a
sConcat (x1, xs1) dt (x2, xs2) = (x1 , xs1 ++ ((dt, x2):xs2))

-- | Refine a stream by establishing the maximum time delta.
--
-- If two samples are separated by a time delta bigger than the given max DT,
-- the former is replicated as many times as necessary.
sRefine :: DTime -> SignalSampleStream a -> SignalSampleStream a
sRefine maxDT (a, as) = (a, sRefineFutureStream maxDT a as)
  where
    sRefineFutureStream :: DTime -> a -> FutureSampleStream a -> FutureSampleStream a
    sRefineFutureStream maxDT _ [] = []
    sRefineFutureStream maxDT a0 ((dt, a):as)
      | dt > maxDT = (maxDT, a0) : sRefineFutureStream maxDT a0 ((dt - maxDT, a):as)
      | otherwise  = (dt, a) : sRefineFutureStream maxDT a as

-- | Refine a stream by establishing the maximum time delta.
--
-- If two samples are separated by a time delta bigger than the given max DT,
-- the auxiliary interpolation function is used to determine the intermendiate
-- sample.
sRefineWith :: (a -> a -> a) -> DTime -> SignalSampleStream a -> SignalSampleStream a
sRefineWith interpolate maxDT (a, as) = (a, refineFutureStreamWith interpolate maxDT a as)
  where
    refineFutureStreamWith :: (a -> a -> a) -> DTime -> a -> FutureSampleStream a -> FutureSampleStream a
    refineFutureStreamWith interpolate maxDT _  [] = []
    refineFutureStreamWith interpolate maxDT a0 ((dt, a):as)
      | dt > maxDT = let a' = interpolate a0 a
                     in (maxDT, interpolate a0 a) : refineFutureStreamWith interpolate maxDT a' ((dt - maxDT, a):as)
      | otherwise  = (dt, a) : refineFutureStreamWith interpolate maxDT a as

-- | Clip a sample stream at a given number of samples.
sClipAfterFrame  :: Int -> SignalSampleStream a -> SignalSampleStream a
sClipAfterFrame  0 (x,_)  = (x, [])
sClipAfterFrame  n (x,xs) = (x, xs')
  where
    xs' = take (n-1) xs

-- | Clip a sample stream after a certain (non-zero) time.
sClipAfterTime   :: DTime -> SignalSampleStream a -> SignalSampleStream a
sClipAfterTime dt (x,xs) = (x, sClipAfterTime' dt xs)
  where
    sClipAfterTime' dt [] = []
    sClipAfterTime' dt ((dt',x):xs)
      | dt < dt'  = []
      | otherwise = ((dt',x):sClipAfterTime' (dt - dt') xs)

-- | Drop the first n samples of a signal stream. The time
-- deltas are not re-calculated.
sClipBeforeFrame :: Int -> SignalSampleStream a -> SignalSampleStream a
sClipBeforeFrame 0 (x,xs) = (x,xs)
sClipBeforeFrame n (x,[]) = (x,[])
sClipBeforeFrame n (_,(dt,x):xs) = sClipBeforeFrame (n-1) (x, xs)

-- | Drop the first samples of a signal stream up to a given time. The time
-- deltas are not re-calculated to match the original stream.
sClipBeforeTime  :: DTime -> SignalSampleStream a -> SignalSampleStream a
sClipBeforeTime dt xs
  | dt <= 0   = xs
  | otherwise = case xs of
                  (x,[])           -> (x,[])
                  (_,(dt',x'):xs') -> if | dt < dt'  -> -- (dt' - dt, x'):xs'
                                                        (x',xs')
                                         | otherwise -> sClipBeforeTime (dt - dt') (x', xs')

-- ** Stream-based evaluation

-- | Evaluate an SF with a 'SignalSampleStream', obtaining an output
-- stream and a continuation.
--
-- You should never use this for actual execution in your applications,
-- only for testing.
evalSF :: SF a b
       -> SignalSampleStream a
       -> (SignalSampleStream b, FutureSF a b)
evalSF sf (a, as) = (outputStrm, fsf')
  where (b,  fsf)  = evalAtZero sf a
        (bs, fsf') = evalFutureSF fsf as
        outputStrm = (b, bs)

-- | Evaluate an initialised SF with a 'FutureSampleStream', obtaining
-- an output stream and a continuation.
--
-- You should never use this for actual execution in your applications,
-- only for testing.
evalFutureSF :: FutureSF a b
             -> FutureSampleStream a
             -> (FutureSampleStream b, FutureSF a b)
evalFutureSF fsf [] = ([], fsf)
evalFutureSF fsf ((dt, a):as) = (outputStrm, fsf'')
  where (b, fsf')   = evalAt fsf dt a
        (bs, fsf'') = evalFutureSF fsf' as
        outputStrm  = (dt, b) : bs
