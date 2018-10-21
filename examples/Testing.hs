{-# LANGUAGE Arrows              #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testing where

-- Examples accompanying the ICFP 2017 paper.
--
-- Changes with respect to the paper:
--
-- - The signature of ballTrulyFalling' in the paper was SF () Double. It's
--   been changed to the intended meaning: TPred ()

-- - The function uniDistStreamMaxDT had the wrong type and the name on the
--   paper was: uniDistStream. This has been fixed.
--

import FRP.Yampa
import FRP.Yampa.Stream
import FRP.Yampa.QuickCheck
import FRP.Yampa.LTLFuture
import Test.QuickCheck

-- * Sample temporal predicates

-- fallingBall :: Double -> SF () Double
-- fallingBall p0 = proc () -> do
--   v <- integral0 -< -9.8
--   p <- integral0 -< v
--   returnA -< (p0 + p)

ballFellLower :: Double -> TPred ()
ballFellLower p0 = Prop (fallingBall p0 >>> arr (\p1 -> p1 <= p0))

-- > evalT (ballFellLower 100) stream01
-- True

ballFallingLower :: Double -> TPred ()
ballFallingLower p0 = Always (ballFellLower p0)

-- > evalT (ballFallingLower 100) stream01
-- True

-- fallingBallPair :: Double -> SF () (Double, Double)
-- fallingBallPair p0 = fallingBall p0 >>> (identity &&& iPre p0)

ballTrulyFalling :: Double -> TPred ()
ballTrulyFalling p0 = Always (Prop (fallingBallPair p0 >>> arr (\(pn, po) -> pn < po)))

-- > evalT (ballTrulyFalling 100) stream01
-- False

ballTrulyFalling' :: Double -> TPred ()
ballTrulyFalling' p0 = Next (Always (Prop (fallingBallPair p0 >>> arr (\(pn, po) -> pn < po))))

-- > evalT (ballTrulyFalling ′ 100) stream01
-- True

bouncingBall :: Double -> Double -> SF () (Double, Double)
bouncingBall p0 v0 = switch (fallingBall'' p0 v0 >>> (identity &&& hit))
                            (\(p0', v0') -> bouncingBall p0' (-v0'))
--
-- fallingBall'' :: Double -> Double -> SF () (Double, Double)
-- fallingBall'' p0 v0 = proc () ->  do
--   v <- arr (v0+) <<< integral -< -9.8
--   p <- arr (p0+) <<< integral -< v
--   returnA -< (p, v)
--
-- hit :: SF (Double, Double) (Event (Double, Double))
-- hit = arr (\(p0, v0) -> if ((p0 <= 0) && (v0 < 0)) then Event (p0, v0) else NoEvent)

ballLower :: Double -> TPred ()
ballLower p0 = Always (Prop (bouncingBall p0 0 >>> arr (\(p1, v1) -> p1 <= p0)))

-- > evalT (ballBouncingLower 100) stream05
-- False

ballBouncingLower = ballLower

ballOverFloor :: Double -> TPred ()
ballOverFloor p0 = Always (Prop (bouncingBall p0 0 >>> arr (\(p1, v1) -> p1 >= 0)))

-- > evalT (ballOverFloor 100) stream05
-- False

fallingBall :: Double -> SF () Double
fallingBall p0 = constant (-9.8) >>> integral0 >>> integral0 >>> arr (+p0)

-- ballFellLower :: Double -> TPred ()
-- ballFellLower p0 = Prop (fallingBall p0, (\_ p1 -> p1 <= p0))

testFellBall = evalT (ballFellLower 100) stream0_1

testFellBall2 = evalT (ballFellLower 100) stream0_2

testFallingBall = evalT (ballFallingLower 100) stream0_1

fallingBallPair :: Double -> SF () (Double, Double)
fallingBallPair p0 = fallingBall p0 >>> (identity &&& iPre p0)

-- ballTrulyFalling :: Double -> TPred ()
-- ballTrulyFalling p0 = Always $ Prop (fallingBallPair p0, \() (pn,po) -> pn < po)

testBallTrulyFalling = evalT (ballTrulyFalling 100) stream0_1

-- ballTrulyFalling' :: Double -> TPred ()
-- ballTrulyFalling' p0 = Next $ Always $ Prop (fallingBallPair p0, \() (pn,po) -> pn < po)

testBallTrulyFalling' = evalT (ballTrulyFalling' 100) stream0_1

fallingBall'' :: Double -> Double -> SF () (Double, Double)
fallingBall'' p0 v0 = proc () -> do
  v <- arr (v0 +) <<< integral -< -9.8
  p <- arr (p0 +) <<< integral -< v
  returnA -< (p, v)

hit :: SF (Double, Double) (Event (Double, Double))
hit = arr (\(p0, v0) -> if (p0 <= 0 && v0 < 0) then Event (p0, v0) else NoEvent)

-- bouncingBall :: Double -> Double -> SF () (Double, Double)
-- bouncingBall p0 v0 = switch (fallingBall'' p0 v0 >>> (identity &&& hit))
--   (\(p0', v0') -> bouncingBall p0' (-v0'))

-- ballBouncingLower :: Double -> TPred ()
-- ballBouncingLower p0 = Always $ Prop (bouncingBall p0 0, (\_ (p1,_) -> p1 <= p0))

testBallBouncing = evalT (ballBouncingLower 100) stream0_5

showBallBouncing = embed (bouncingBall 100 0 >>> arr fst ) ((), map (second Just) [(0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()),(0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ())])

-- ballOverFloor :: Double -> TPred ()
-- ballOverFloor p0 = Always $ Prop (bouncingBall p0 0, (\_ (p1, v1) -> p1 >= 0))

testBallOverFloor = evalT (ballOverFloor 100) stream0_5'

showBallBouncing1 = embed (bouncingBall 110.24999999999999 0 >>> arr fst ) ((), map (second Just) [(0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()),(0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ())])

testBallOverFloor' = evalT (ballOverFloor 110.24999999999999) stream0_5'

propTestBallOverFloor =
  forAll myStream (evalT (ballOverFloor 110.24999999999999))
 where  myStream :: Gen (SignalSampleStream ())
        myStream = uniDistStream

propTestBallOverFloorFixed =
  forAll myStream (evalT (ballOverFloor 110.24999999999999))
 where  myStream :: Gen (SignalSampleStream ())
        myStream = fixedDelayStream (1/60)

bouncingBall' p0 v0 = bouncingBall p0 v0 >>> arr fst

ballAboveFloor :: Double -> Double -> SF () (Double, Bool)
ballAboveFloor p0 v0 = proc () -> do
  ballPos <- bouncingBall' p0 v0 -< ()
  let aboveFloor = ballPos >= 0
  returnA -< (ballPos, aboveFloor)

-- * Sample streams

stream0_1 = ((), [(0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ())])

stream0_2 = ((), [(0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (-1000000, ())])

stream0_5 = ((), [(0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()),(0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ())])

stream0_5' = ((), [(0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()),(0.5, ())])


-- ** Extended SFs

integral0 = imIntegral 0

-- * Talk

greaterThan :: SF (Int, Int) Bool
greaterThan = arr $ \(x,y) -> x > y

alwaysGreater :: TPred (Int, Int)
alwaysGreater = Always $ Prop greaterThan

-- > evalT alwaysGreater ((5,1), [(0.001, (6, 1)), (0.001, (9, 2))])
-- True
eval1 = evalT alwaysGreater ((5,1), [(0.001, (6, 1)), (0.001, (9, 2))])

-- > evalT alwaysGreater ((1,5), [(0.001, (6, 1)), (0.001, (9, 2))])
-- False
eval2 = evalT alwaysGreater ((1,5), [(0.001, (6, 1)), (0.001, (9, 2))])

alwaysGreaterProperty :: Property
alwaysGreaterProperty = forAll arbitrary (evalT alwaysGreater)

evalQ1 = quickCheck alwaysGreaterProperty
