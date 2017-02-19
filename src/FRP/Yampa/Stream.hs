module FRP.Yampa.Stream where

import FRP.Yampa.InternalCore

-- * Streams
type FutureSampleStream a = [(DTime, a)]
type SignalSampleStream a = (a, FutureSampleStream a)
