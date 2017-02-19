-- Why is this extra module necessary?
module SampleStreams where

------------------------------------------------------------------------------
import FRP.Yampa
import FRP.Yampa.Stream
import FRP.Yampa.Testing

-- * Streams

-- Stream samples, or samples with time information
newtype TimedSample a = TimedSample { unSample :: (DTime, a) }
 deriving (Eq, Show)

-- | A whole testing sample stream, with an initial sample
-- and a stream of timed samples.
type TestSampleStream a = (a, [TimedSample a])

-- | Turn a stream with timedSamples into a plan stream with
-- pairs of deltas and values.
adaptTestStream :: TestSampleStream a -> SignalSampleStream a
adaptTestStream (x, xs) = (x, map unSample xs)

-- | Turn a stream with sampling times into a list of values.
samples :: SignalSampleStream a -> [a]
samples (a, as) = a : map snd as
