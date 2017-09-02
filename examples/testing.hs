{-# LANGUAGE Arrows     #-}
{-# LANGUAGE MultiWayIf #-}

-- Examples accompanying the ICFP 2017 paper.
--
-- Changes with respect to the paper:
--
-- - The signature of ballTrulyFalling' in the paper was SF () Double. It's
--   been changed to the intended meaning: TPred ()

-- - The function uniDistStreamMaxDT had the wrong type and the name on the
--   paper was: uniDistStream. This has been fixed.
--

import AFRPTestsCommon
import Control.Arrow
import Distribution.TestSuite.QuickCheck
import FRP.Yampa
import FRP.Yampa.Stream
import FRP.Yampa.Testing
import SampleStreams
import SampleStreamsQC
import TemporalLogic
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Property

-- type SignalSampleStream a = (a, FutureSampleStream a)
-- type FutureSampleStream a = [ (DTime, a) ]

-- evalT :: TPred a -> SignalSampleStream a -> Bool

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

propReverseTwice :: [Int ] -> Property
propReverseTwice xs = property $ reverse (reverse xs) == xs
-- > quickCheck propReverseTwice
-- + + + OK, passed 100 tests ◦

propReverseOnce :: [Int ] -> Property
propReverseOnce xs = property $ reverse xs == xs

-- > quickCheck propReverseOnce
-- ∗∗∗ Failed ! Falsifiable (after 3 tests and 1 shrink) :
-- [1, 0]

generateDeltas :: Distribution -> Range -> Length -> Gen DTime
generateDeltas = undefined

data Distribution = DistConstant | DistNormal (DTime, DTime) | DistRandom
type Range = (Maybe DTime, Maybe DTime)
type Length = Maybe (Either Int DTime)

generateStream     :: Arbitrary a => Distribution -> Range -> Length -> Gen (SignalSampleStream a)
generateStream     = undefined

generateStreamWith :: Arbitrary a => (Int -> DTime -> Gen a) -> Distribution -> Range -> Length -> Gen (SignalSampleStream a)
generateStreamWith = undefined

-- uniDistStream            :: Arbitrary a => Gen (SignalSampleStream a)
-- uniDistStream            = generateStream DistRandom (Nothing, Nothing) Nothing
uniDistStreamMaxDT       :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
uniDistStreamMaxDT maxDT = generateStream DistRandom (Nothing, Just maxDT ) Nothing
-- fixedDelayStream         :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
-- fixedDelayStream dt      = generateStream DistConstant (Just dt, Just dt) Nothing

sConcat :: SignalSampleStream a -> DTime -> SignalSampleStream a -> SignalSampleStream a
sConcat (x1, xs1) dt (x2, xs2) = (x1 , xs1 ++ ((dt, x2):xs2))

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

sClipAfterFrame  :: Int -> SignalSampleStream a -> SignalSampleStream a
sClipAfterFrame  0 (x,_)  = (x, [])
sClipAfterFrame  n (x,xs) = (x, xs')
  where
    xs' = take (n-1) xs

sClipAfterTime   :: DTime -> SignalSampleStream a -> SignalSampleStream a
sClipAfterTime dt (x,xs) = (x, sClipAfterTime' dt xs)
  where
    sClipAfterTime' dt [] = []
    sClipAfterTime' dt ((dt',x):xs)
      | dt < dt'  = []
      | otherwise = ((dt',x):sClipAfterTime' (dt - dt') xs)

sClipBeforeFrame :: Int -> SignalSampleStream a -> SignalSampleStream a
sClipBeforeFrame 0 (x,xs) = (x,xs)
sClipBeforeFrame n (x,[]) = (x,[])
sClipBeforeFrame n (_,(dt,x):xs) = sClipBeforeFrame (n-1) (x, xs)

sClipBeforeTime  :: DTime -> SignalSampleStream a -> SignalSampleStream a
sClipBeforeTime dt xs
  | dt <= 0   = xs
  | otherwise = case xs of
                  (x,[])           -> (x,[])
                  (_,(dt',x'):xs') -> if | dt < dt'  -> -- (dt' - dt, x'):xs'
                                                        (x',xs')
                                         | otherwise -> sClipBeforeTime (dt - dt') (x', xs')

integral0 = imIntegral 0

stream0_1 = ((), [(0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ())])

stream0_2 = ((), [(0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (0.1, ()), (-1000000, ())])

stream0_5 = ((), [(0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()),(0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ())])

stream0_5' = ((), [(0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()), (0.5, ()),(0.5, ())])

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

-- alwaysEqual sf1 sf2 =
--   Always $ Prop (sf1 &&& sf2, (\_ (o1, o2) -> o1 == o2))
alwaysEqual sf1 sf2 =
  Always $ Prop ((sf1 &&& sf2) >>> arr (\(o1, o2) -> o1 == o2))

propTestAlwaysEqual =
  forAll myStream (evalT (alwaysEqual f1 f2))
 where  myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream
        f1 :: SF Double Double
        f1 = arr (**2)
        f2 :: SF Double Double
        f2 = arr (^2)

propTestAlwaysEqual2 =
  forAll dataStream $ \s ->
    forAll functionStream $ \f -> 
      evalT (alwaysEqual (arr (apply f) >>> identity) (arr (apply f))) s
 where  dataStream :: Gen (SignalSampleStream Int)
        dataStream = uniDistStream

        functionStream :: Gen (Fun Int Int)
        functionStream = arbitrary

type SPred a = SF a Bool

notSF     sf        = sf >>> arr (not)
andSF     sf1 sf2   = (sf1 &&& sf2) >>> arr (uncurry (&&))
orSF      sf1 sf2   = (sf1 &&& sf2) >>> arr (uncurry (||))
implySF   sf1 sf2   = orSF sf2 (notSF sf1)

history :: SPred a -> SPred a
history sf = loopPre True $ proc (a, last) -> do
  b <- sf -< a
  let cur = last && b
  returnA -< (cur, cur)

ever :: SPred a -> SPred a
ever sf = loopPre False $ proc (a, last) -> do
  b <- sf -< a
  let cur = last || b
  returnA -< (cur, cur)

bouncingBall' p0 v0 = bouncingBall p0 v0 >>> arr fst

ballAboveFloor :: Double -> Double -> SF () (Double, Bool)
ballAboveFloor p0 v0 = proc () -> do
  ballPos <- bouncingBall' p0 v0 -< ()
  let aboveFloor = ballPos >= 0
  returnA -< (ballPos, aboveFloor)
