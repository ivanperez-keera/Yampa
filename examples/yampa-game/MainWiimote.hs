{-# LANGUAGE Arrows #-}
import Graphics.UI.SDL as SDL
import System.Exit
import System.Random
import FRP.Yampa as Yampa
import FRP.Yampa.Physics
import Data.IORef

width = 640
height = 480
type Radius  = Float

type TotalTime = IORef Int

initGraphs = do
       SDL.init [InitVideo]
       screen <- setVideoMode 640 480 16 [SWSurface]
       setCaption "Test" ""
       enableUnicode True

display :: Shape -> IO() -- Surface -> IO ()
display (Shape x y w h) -- image
    = do screen <- getVideoSurface
         let format = surfaceGetPixelFormat screen
         red   <- mapRGB format 0xFF 0 0xAA 
         green <- mapRGB format 0 0xFF 0
         fillRect screen Nothing green
         fillRect screen (Just (Rect x y w h)) red
         SDL.flip screen

main = do
  -- initGraphs
  time <- newIORef (0 :: Int)
  reactimate (initGraphs >> return Yampa.NoEvent)
             (\_ -> do
                e <- getEvent
                ts <- fmap fromIntegral getTicks
                pt <- readIORef time
                let dt  = ts - pt
                    dtY = fromIntegral dt / 100 
                writeIORef time ts
                return (dtY, Just e)) -- 10ms, no input
             (\_ e -> do
                case e of
                  Just e' -> display e' >> return False
                  _       -> return True)
             signalSF

data InputAction = Close
                 | MouseMove Int Int

type Output = Maybe Shape
data Shape = Shape Int Int Int Int

-- signalSF :: SF Int Int
-- signalSF = (maybeCloseSF &&& mouseFollowingSF) >>> mergeResults
signalSF = arr (const ()) >>> updateG

maybeCloseSF :: SF (Yampa.Event InputAction) (Yampa.Event Output)
maybeCloseSF = filterClose >>> arr (`tag` Nothing)

mouseFollowingSF :: SF (Yampa.Event InputAction) Output
mouseFollowingSF = filterMouseMove >>> computeMouseMove >>> drawShape >>> arr Just

mergeResults :: SF (Yampa.Event Output, Output) Output
mergeResults = arr mergeResults'
  where mergeResults' (Yampa.NoEvent, e) = e
        mergeResults' (Event e, _) = e

filterClose = arr (filterE isClose)

computeMouseMove = hold (MouseMove 0 0)

filterMouseMove = arr $ filterE isMouseMove

isMouseMove :: InputAction -> Bool
isMouseMove (MouseMove _ _) = True
isMouseMove _               = False

isClose :: InputAction -> Bool
isClose Close = True
isClose _     = False

drawShape = arr (\(MouseMove x y) -> Shape x y 30 30)

getEvent :: IO (Yampa.Event InputAction)
getEvent 
    = do event <- pollEvent
         case event of
           Quit                     -> return (Event Close)
           KeyDown (Keysym _ _ 'q') -> return (Event Close)
           MouseMotion x y z w      -> return (Event (MouseMove (fromIntegral x) (fromIntegral y)))
           _                        -> return Yampa.NoEvent

data Bounds = Bounds {
  xMin  :: Float,
  yMin  :: Float,
  xMax  :: Float,
  yMax  :: Float
}

---- The boolean event says whether there has been a 'pat' on the ball or not.
---- Right now, I don't want to complicate this.
--discardInputs :: SF (Event ()) ()
--discardInputs = arr $ const ()

-- I just want to process a spacebar!
-- These are ***pointwise*** bindings.
-- TODO Abstract out the initial state.
updateG :: SF () (Maybe Shape)
updateG = proc () -> do
  ((x,y),_vel) <- bouncingBall (0,200) (40,45) (cor defPhysics) ballRadius bounds -< ()
  returnA  -< Just $ Shape (fI (x-ballRadius)) (fI (maxY-(y-ballRadius))) (fI (2*ballRadius)) (fI (2*ballRadius))
  where
    bounds = Bounds { xMin = 0, yMin = 0, xMax = 640, yMax = 480 }
    ballRadius = 30
    ballColour = (0.7, 0.7, 0.7)
    fI = round
    maxY = 480 - 2 * ballRadius

-- fallingBall' pos0 vel0 radius boundaries ground

-- This time around, we detect for collisions with the ground given by 'ground',
-- and record the position, velocity, and the direction normal to the
-- at the location of the collision.
-- Carries out a check for each of the four bounds
fallingBall' :: Pos2 -> Vel2 -> Radius -> Bounds
             -> SF () ((Pos2, Vel2), Yampa.Event (Dir2,(Pos2,Vel2)))
fallingBall' p0 v0 rad bounds = proc () -> do
  pv@(p,_v) <- freeFall p0 v0 -< ()
  hitXMin  <- edgeTag (1, 0) -< fst p <= xMin bounds + rad
  hitYMin  <- edgeTag (0, 1) -< snd p <= yMin bounds + rad
  hitXMax  <- edgeTag (-1,0) -< fst p >= xMax bounds - rad
  hitYMax  <- edgeTag (0,-1) -< snd p >= yMax bounds - rad
  let hitInfo = foldr1 (mergeBy mergeHits) [hitXMin,hitYMin,hitXMax,hitYMax]
  returnA -< (pv, hitInfo `attach` pv)
  where
    mergeHits = (^+^) -- simply add the two collision directions together.

-- We use the extra collision information as given by fallingBall' to compute
-- the new bouncing ball's velocity.
-- Bounces off all four sides of the screen!
bouncingBall :: Pos2 -> Vel2 -> COR -> Radius -> Bounds -> SF () (Pos2,Vel2)
bouncingBall p0 v0 cor rad bounds = bouncingBall' p0 v0
  where
    bouncingBall' p0 v0 =
      switch (fallingBall' p0 v0 rad bounds) $
        \(dir,(p,v)) -> bouncingBall' p (reflect dir ((-cor) *^ v))
    reflect l v = (2*(v `dot` l)/(l `dot` l)) *^ l ^-^ v
