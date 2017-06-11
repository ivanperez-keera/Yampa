{-# LANGUAGE Arrows #-}
import Graphics.UI.SDL as SDL
import FRP.Yampa       as Yampa
import Data.IORef
import Debug.Trace
import YampaSDL

width  = 640
height = 480

main = do
  timeRef <- yampaSDLTimeInit
  reactimate initGraphs
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                return (dtSecs, Nothing))
             (\_ e -> display e >> return False)
             (bounce (fromIntegral height / 2) 0)

-- The FRP stuff
falling :: Double -> Double -> SF () (Double, Double)
falling y0 v0 = proc () -> do
  vy <- (v0+) ^<< integral -< gravity
  py <- (y0+) ^<< integral -< vy
  returnA -< (py, vy)

bounce :: Double -> Double -> SF () (Double, Double)
bounce y vy = switch (falling y vy >>> (Yampa.identity &&& hitBottom))
                     (\(y, vy) -> bounce y (-vy))

hitBottom :: SF (Double, Double) (Yampa.Event (Double, Double))
hitBottom = arr (\(y,vy) ->
                  let boxTop = y + fromIntegral boxSide
                  in if (boxTop > fromIntegral height) && (vy > 0)
                       then Yampa.Event (y, vy)
                       else Yampa.NoEvent)

-- The SDL stuff
initGraphs :: IO ()
initGraphs = do
  -- Initialise SDL
  SDL.init [InitVideo]

  -- Create window
  screen <- setVideoMode width height 16 [SWSurface]
  setCaption "Test" ""

display :: (Double, Double) -> IO()
display (boxY,_) = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  green <- mapRGB format 0 0xFF 0
  fillRect screen Nothing green

  -- Paint small red square, at an angle 'angle' with respect to the center
  red <- mapRGB format 0xFF 0 0
  let x = (width - boxSide) `div` 2
      y = round boxY
  fillRect screen (Just (Rect x y boxSide boxSide)) red

  -- Double buffering
  SDL.flip screen

gravity :: Double
gravity = 6.2

boxSide :: Int
boxSide = 30
