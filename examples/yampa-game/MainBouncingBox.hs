{-# LANGUAGE Arrows #-}
import Data.IORef
import Debug.Trace
import FRP.Yampa       as Yampa
import Graphics.UI.SDL as SDL

-- Helper functions
import YampaSDL

width :: Num a => a
width  = 640
height :: Num a => a
height = 480

-- | Reactimation.
--
-- This main function runs an FRP system by producing a signal, passing it
-- through a signal function, and consuming it.
--
-- The first two arguments to reactimate are the value of the input signal
-- at time zero and at subsequent times, together with the times between
-- samples.
-- 
-- The third argument to reactimate is the output consumer that renders
-- the signal.
--
-- The last argument is the actual signal function.
--
main = do
  timeRef <- yampaSDLTimeInit
  reactimate initGraphs
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                return (dtSecs, Nothing))
             (\_ e -> display e >> return False)
             (bounce (fromIntegral height / 2) 0)

-- * FRP stuff

-- | Vertical coordinate and velocity of a falling mass starting
-- at a height with an initial velocity.
falling :: Double -> Double -> SF () (Double, Double)
falling y0 v0 = proc () -> do
  vy <- (v0+) ^<< integral -< gravity
  py <- (y0+) ^<< integral -< vy
  returnA -< (py, vy)

-- | Vertical coordinate and velocity of a bouncing mass starting
-- at a height with an initial velicity.
bounce :: Double -> Double -> SF () (Double, Double)
bounce y vy = switch (falling y vy >>> (Yampa.identity &&& hitBottom))
                     (\(y, vy) -> bounce y (-vy))

-- | Fire an event when the input height and velocity indicate
-- that the object has hit the bottom (so it's falling and the
-- vertical position is under the floor).
hitBottom :: SF (Double, Double) (Yampa.Event (Double, Double))
hitBottom = arr (\(y,vy) ->
                  let boxTop = y + fromIntegral boxSide
                  in if (boxTop > fromIntegral height) && (vy > 0)
                       then Yampa.Event (y, vy)
                       else Yampa.NoEvent)

-- * Graphics

-- | Initialise rendering system.
initGraphs :: IO ()
initGraphs = do
  -- Initialise SDL
  SDL.init [InitVideo]

  -- Create window
  screen <- setVideoMode width height 16 [SWSurface]
  setCaption "Test" ""

-- | Display a box at a position.
display :: (Double, Double) -> IO()
display (boxY,_) = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format 55 60 64
  fillRect screen Nothing bgColor

  -- Paint small red square, at an angle 'angle' with respect to the center
  foreC <- mapRGB format 212 108 73
  let x = (width - boxSide) `div` 2
      y = round boxY
  fillRect screen (Just (Rect x y boxSide boxSide)) foreC

  -- Double buffering
  SDL.flip screen

gravity :: Double
gravity = 6.2

boxSide :: Int
boxSide = 30
