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
import Control.Arrow
import FRP.Yampa
import TemporalLogic
import FRP.Yampa.Stream
import Test.QuickCheck
import Test.QuickCheck.Property

-- type SignalSampleStream a = (a, FutureSampleStream a)
-- type FutureSampleStream a = [ (DTime, a) ]

-- evalT :: TPred a -> SignalSampleStream a -> Bool

fallingBall :: Double -> SF () Double
fallingBall p0 = proc () -> do
  v <- integral0 -< -9.8
  p <- integral0 -< v
  returnA -< (p0 + p)

ballFellLower :: Double -> TPred ()
ballFellLower p0 = Prop (fallingBall p0 >>> arr (\p1 -> p1 <= p0))

-- > evalT (ballFellLower 100) stream01
-- True

ballFallingLower :: Double -> TPred ()
ballFallingLower p0 = Always (ballFellLower p0)

-- > evalT (ballFallingLower 100) stream01
-- True

fallingBallPair :: Double -> SF () (Double, Double)
fallingBallPair p0 = fallingBall p0 >>> (identity &&& iPre p0)

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

fallingBall'' :: Double -> Double -> SF () (Double, Double)
fallingBall'' p0 v0 = proc () ->  do
  v <- arr (v0+) <<< integral -< -9.8
  p <- arr (p0+) <<< integral -< v
  returnA -< (p, v)

hit :: SF (Double, Double) (Event (Double, Double))
hit = arr (\(p0, v0) -> if ((p0 <= 0) && (v0 < 0)) then Event (p0, v0) else NoEvent)

ballLower :: Double -> TPred ()
ballLower p0 = Always (Prop (bouncingBall p0 0 >>> arr (\(p1, v1) -> p1 <= p0)))

-- > evalT (ballBouncingLower 100) stream05
-- False

ballOverFloor :: Double -> TPred ()
ballOverFloor p0 = Always (Prop (bouncingBall p0 0 >>> arr (\(p1, v1) -> p1 >= 0)))

-- > evalT (ballOverFloor 100) stream05
-- False

integral0 = integral

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

uniDistStream            :: Arbitrary a => Gen (SignalSampleStream a)
uniDistStream            = generateStream DistRandom (Nothing, Nothing) Nothing
uniDistStreamMaxDT       :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
uniDistStreamMaxDT maxDT = generateStream DistRandom (Nothing, Just maxDT ) Nothing
fixedDelayStream         :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
fixedDelayStream dt      = generateStream DistConstant (Just dt, Just dt) Nothing

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
