{-# LANGUAGE Arrows #-}
import Control.Monad
import Data.IORef
import Data.Maybe
import Debug.Trace
import FRP.Yampa       as Yampa
import Graphics.UI.SDL as SDL
import System.CWiid

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
  mWiimote <- initializeWiimote
  timeRef <- newIORef (0 :: Int)
  if isNothing mWiimote
    then putStrLn "Couldn't find wiimote"
    else do let wiimote = fromJust mWiimote
            reactimate (initGraphs >> senseWiimote wiimote)
                       (\_ -> do
                          dtSecs <- yampaSDLTimeSense timeRef
                          mInput <- senseWiimote wiimote
                          return (dtSecs, Just mInput)
                       )
                       (\_ e -> display (e) >> return False)
                       player

-- Pure SF
inCircles :: SF (Double, Double) (Double, Double)
inCircles = proc (centerX, centerY) -> do
  t <- time -< ()
  let x      = centerX + cos t * radius
      y      = centerY + sin t * radius
      radius = 30
  returnA -< (x,y)

-- * Graphics

-- | Initialise rendering system.
initGraphs :: IO ()
initGraphs = do
  -- Initialise SDL
  SDL.init [InitVideo]

  -- Create window
  screen <- SDL.setVideoMode width height 16 [SWSurface]
  SDL.setCaption "Test" ""

-- | Display a box at a position.
display :: (Double, Double) -> IO()
display (playerX, playerY) = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format 55 60 64
  fillRect screen Nothing bgColor

  -- Paint small red square, at an angle 'angle' with respect to the center
  foreC <- mapRGB format 212 108 73
  let side = 30
      x = round playerX
      y = round playerY
  fillRect screen (Just (Rect x y side side)) foreC

  -- Double buffering
  SDL.flip screen

player :: SF (Double, Double) (Double, Double)
player = inCircles

senseWiimote :: CWiidWiimote -> IO (Double, Double)
senseWiimote wmdev = do
  irs   <- cwiidGetIR wmdev

  -- Obtain positions of leds 1 and 2 (with a normal wii bar, those
  -- will be the ones we use).
  let led1   = irs!!0
      led2   = irs!!1

  -- Calculate mid point between sensor bar leds
  let posX = ((cwiidIRSrcPosX led1) + (cwiidIRSrcPosX led2)) `div` 2
      posY = ((cwiidIRSrcPosY led1) + (cwiidIRSrcPosY led2)) `div` 2

  -- Calculate proportional coordinates
  let propX = fromIntegral (1024 - posX) / width
      propY = fromIntegral (max 0 (posY - 384)) / 384.0

  -- Calculate game area coordinates
  let finX  = width  * propX
      finY  = height * propY

  return (finX, finY) 

-- | Initializes the wiimote, optionally returning the sensing function. It
-- returns Nothing if the Wiimote cannot be detected. Users should have a BT
-- device and press 1+2 to connect to it. A message is shown on stdout.
initializeWiimote :: IO (Maybe CWiidWiimote)
initializeWiimote = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  case wm of
    Nothing  -> return ()
    Just wm' -> void $ cwiidSetRptMode wm' 15 -- Enable button reception, acc and IR
  return wm
