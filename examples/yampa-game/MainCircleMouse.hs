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
  timeRef <- newIORef (0 :: Int)
  controllerRef <- newIORef $ Controller (0,0)
  reactimate (initGraphs >> readIORef controllerRef)
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                mInput <- sdlGetController controllerRef
                -- print (mInput)
                return (dtSecs, Just mInput)
             )
             (\_ e -> display (e) >> return False)
             player

-- * FRP stuff

-- | Player is going in circles around the input controller position
player :: SF Controller (Double, Double)
player = arr controllerPos >>> inCircles

-- | Coordinate of a body going in circles around another body.
inCircles :: SF (Double, Double) (Double, Double)
inCircles = proc (centerX, centerY) -> do
  t <- time -< ()
  let x      = centerX + cos t * radius
      y      = centerY + sin t * radius
      radius = 30
  returnA -< (x,y)

-- * SDL stuff

-- ** Input subsystem

-- | Input controller
data Controller = Controller
 { controllerPos   :: (Double, Double)
 }

-- | Give a controller, refresh its state and return the latest value.
-- We need a non-blocking controller-polling function.
sdlGetController :: IORef Controller -> IO Controller
sdlGetController controllerState = do
  state <- readIORef controllerState
  e     <- pollEvent
  case e of
    MouseMotion x y _ _ -> writeIORef controllerState (Controller (fromIntegral x, fromIntegral y)) >> sdlGetController controllerState
    _                   -> return state

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
  let side = 10
      x = round playerX
      y = round playerY
  fillRect screen (Just (Rect x y side side)) foreC

  -- Double buffering
  SDL.flip screen
