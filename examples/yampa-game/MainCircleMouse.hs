{-# LANGUAGE Arrows #-}
import Graphics.UI.SDL as SDL
import FRP.Yampa       as Yampa
import Data.IORef
import Debug.Trace

width  = 640
height = 480

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

-- | Updates the time in an IO Ref and returns the time difference
updateTime :: IORef Int -> Int -> IO Int
updateTime timeRef newTime = do
  previousTime <- readIORef timeRef
  writeIORef timeRef newTime
  return (newTime - previousTime)

yampaSDLTimeSense :: IORef Int -> IO Yampa.DTime
yampaSDLTimeSense timeRef = do
  -- Get time passed since SDL init
  newTime <- fmap fromIntegral SDL.getTicks

  -- Obtain time difference
  dt <- updateTime timeRef newTime
  let dtSecs = fromIntegral dt / 100
  return dtSecs

-- Pure SF
inCircles :: SF (Double, Double) (Double, Double)
inCircles = proc (centerX, centerY) -> do
  t <- time -< ()
  let x      = centerX + cos t * radius
      y      = centerY + sin t * radius
      radius = 30
  returnA -< (x,y)

-- We need a non-blocking controller-polling function.
sdlGetController :: IORef Controller -> IO Controller
sdlGetController controllerState = do
  state <- readIORef controllerState
  e    <- pollEvent
  case e of
    MouseMotion x y _ _ -> writeIORef controllerState (Controller (fromIntegral x, fromIntegral y)) >> sdlGetController controllerState
    _                   -> return state

-- TODO: Consider using
getMousePos :: IO (Double, Double)
getMousePos = do
  pumpEvents
  (x,y,_) <- SDL.getMouseState
  return (fromIntegral x, fromIntegral y)

data Controller = Controller
 { controllerPos   :: (Double, Double)
 }

initGraphs :: IO ()
initGraphs = do
  -- Initialise SDL
  SDL.init [InitVideo]

  -- Create window
  screen <- SDL.setVideoMode width height 16 [SWSurface]
  SDL.setCaption "Test" ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

display :: (Double, Double) -> IO()
display (playerX, playerY) = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  green <- mapRGB format 0 0xFF 0
  fillRect screen Nothing green

  -- Paint small red square, at an angle 'angle' with respect to the center
  red <- mapRGB format 0xFF 0 0
  let side = 10
      x = round playerX
      y = round playerY
  fillRect screen (Just (Rect x y side side)) red

  -- Double buffering
  SDL.flip screen

player :: SF Controller (Double, Double)
player = arr controllerPos >>> inCircles
