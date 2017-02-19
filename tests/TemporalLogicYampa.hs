{-# LANGUAGE Arrows #-}
module TemporalLogicYampa where

------------------------------------------------------------------------------
import FRP.Yampa as Yampa
import FRP.Yampa.Stream
import FRP.Yampa.Testing
import SampleStreams

sofarSF :: SF Bool Bool
sofarSF = loopPre True $ arr $ \(n,o) -> let n' = o && n in (n', n')

everSF :: SF Bool Bool
everSF = loopPre False $ arr $ \(n,o) -> let n' = o || n in (n', n')

untilSF :: SF (Bool, Bool) Bool
untilSF = switch
  (loopPre True $ arr (\((i,u),o) -> let n = o && i
                                     in ((n, if (o && u) then Event () else NoEvent), n)))
  (\_ -> arr snd >>> sofarSF)

lastSF :: SF Bool Bool
lastSF = iPre False

andSF :: SF (Bool, Bool) Bool
andSF = arr (uncurry (&&))

orSF :: SF (Bool, Bool) Bool
orSF = arr (uncurry (||))

notSF :: SF Bool Bool
notSF = arr not

impliesSF :: SF (Bool, Bool) Bool
impliesSF = arr $ \(i,p) -> not i || p

data UnclearResult = Possibly Bool | Definitely Bool

causally :: SF a Bool -> SF a UnclearResult
causally = (>>> arr Definitely)

data TSF a = NonCausal (SF a UnclearResult)
           | Causal    (SF a Bool)

evalTSF :: TSF a -> SignalSampleStream a -> Bool
evalTSF (Causal sf)    ss = firstSample $ fst $ evalSF sf ss
evalTSF (NonCausal sf) ss = clarifyResult $ lastSample $ fst $ evalSF sf

clarifyResult :: UnclearResult -> Bool
clarifyResult (Possibly x)   = x
clarifyResult (Definitely x) = x

firstSample :: SignalSampleStream a -> a
firstSample = fst

lastSample :: SignalSampleStream a -> a
lastSample (a, []) = a
lastSample (_, ((_,x):xs)) = lastSample x xs
