{-# LANGUAGE Arrows #-}
module FRP.Yampa.LTLPast where

------------------------------------------------------------------------------
import FRP.Yampa

-- * SFs that implement temporal combinators

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

-- * SF combinators that implement temporal combinators

type SPred a = SF a Bool

notSF' :: SPred a -> SPred a
notSF' sf = sf >>> arr (not)

andSF' :: SPred a -> SPred a -> SPred a
andSF' sf1 sf2 = (sf1 &&& sf2) >>> arr (uncurry (&&))

orSF' :: SPred a -> SPred a -> SPred a
orSF' sf1 sf2 = (sf1 &&& sf2) >>> arr (uncurry (||))

implySF' :: SPred a -> SPred a -> SPred a
implySF' sf1 sf2 = orSF' sf2 (notSF' sf1)

history' :: SPred a -> SPred a
history' sf = loopPre True $ proc (a, last) -> do
  b <- sf -< a
  let cur = last && b
  returnA -< (cur, cur)

ever' :: SPred a -> SPred a
ever' sf = loopPre False $ proc (a, last) -> do
  b <- sf -< a
  let cur = last || b
  returnA -< (cur, cur)
