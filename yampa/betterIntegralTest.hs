{-# LANGUAGE Arrows #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import FRP.Yampa.Integration
import FRP.Yampa

import IHaskell.Display.Diagrams ()
import Diagrams.Backend.Cairo (B)
import Plots (r2AxisMain, linePlot', Axis, r2Axis)
import Diagrams.Prelude hiding (Time, (^/), (^+^), trace, coords, (*^))
import System.Environment


fallingBall :: Double -> Double -> SF () (Double, Double)
fallingBall p0 v0 = proc () -> do
  let g = -9.81
  v <- arr (\x -> v0 + x) <<< integral -< g
  p <- arr (\y -> p0 + y) <<< integral -< v
  returnA -< (p,v)

bouncingBall :: Double -> Double -> SF () (Double, Double)
bouncingBall p0 v0 =
  switch (fallingBall p0 v0 >>> (arr id &&& hitFloor))
         (\(p,v) -> bouncingBall p (-v))

hitFloor :: SF (Double, Double) (Event (Double, Double))
hitFloor = arr $ \(p,v) ->
  if p < 0 && v < 0 then Event (p,v) else noEvent

test :: [(Double, Double)]
test = embed (bouncingBall 10.0 10.0) ((), map (\n -> (0.0001 * fromIntegral n, Just ())) [0 :: Int, 1 .. 1000])

main :: IO ()
main = do
  withArgs ["-obouncingBallNew.png"] (r2AxisMain jSaxis)

jSaxis :: Axis B V2 Double
jSaxis = r2Axis &~ do
  let ts = map (* 0.0001) [0, 1 ..]
  linePlot' $ take 1000 $ zip ts (map fst test)
