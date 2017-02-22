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
