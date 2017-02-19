{-# LANGUAGE PatternGuards #-}
module FRP.Yampa.Stream where

import FRP.Yampa.InternalCore

-- * Streams
type FutureSampleStream a = [(DTime, a)]
type SignalSampleStream a = (a, FutureSampleStream a)

mergeStreams :: (a -> a -> a) -> SignalSampleStream a -> SignalSampleStream a -> SignalSampleStream a
mergeStreams f (a1, as1) (a2, as2) =
  (f a1 a2, mergeFutureStreams f as1 as2)

mergeFutureStreams :: (a -> a -> a) -> FutureSampleStream a -> FutureSampleStream a -> FutureSampleStream a
mergeFutureStreams f [] as = as
mergeFutureStreams f as [] = as
mergeFutureStreams f ((dt1, a1):as1) ((dt2, a2):as2)
  | dt1 == dt2 
  = ((dt1, f a1 a2):mergeFutureStreams f as1 as2)
  | dt1 < dt2 
  , let dd = dt2-dt1
  = ((dt1, a1):mergeFutureStreams f as1 ((dd, a2):as2))
  | dt1 > dt2 
  , let dd = dt1-dt2
  = ((dt2, a2):mergeFutureStreams f ((dd, a1):as1) as2)

concatStreams :: SignalSampleStream a -> FutureSampleStream a -> SignalSampleStream a
concatStreams (a1, as1) as2 = (a1, concatFutureStreams as1 as2)

concatFutureStreams :: FutureSampleStream a -> FutureSampleStream a -> FutureSampleStream a
concatFutureStreams as1 as2 = as1 ++ as2

refineStream :: DTime -> SignalSampleStream a -> SignalSampleStream a
refineStream maxDT (a, as) = (a, refineFutureStream maxDT a as)

refineFutureStream :: DTime -> a -> FutureSampleStream a -> FutureSampleStream a
refineFutureStream maxDT _ [] = []
refineFutureStream maxDT a0 ((dt, a):as)
  | dt > maxDT = (maxDT, a0) : refineFutureStream maxDT a0 ((dt - maxDT, a):as)
  | otherwise  = (dt, a) : refineFutureStream maxDT a as

refineStreamWith :: (a -> a -> a) -> DTime -> SignalSampleStream a -> SignalSampleStream a
refineStreamWith interpolate maxDT (a, as) = (a, refineFutureStreamWith interpolate maxDT a as)

refineFutureStreamWith :: (a -> a -> a) -> DTime -> a -> FutureSampleStream a -> FutureSampleStream a
refineFutureStreamWith interpolate maxDT _  [] = []
refineFutureStreamWith interpolate maxDT a0 ((dt, a):as)
  | dt > maxDT = let a' = interpolate a0 a
                 in (maxDT, interpolate a0 a) : refineFutureStreamWith interpolate maxDT a' ((dt - maxDT, a):as)
  | otherwise  = (dt, a) : refineFutureStreamWith interpolate maxDT a as
