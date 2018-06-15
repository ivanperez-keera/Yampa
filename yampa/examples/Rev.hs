{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Arrows #-}
import Graphics.UI.SDL as SDL
import FRP.Yampa       as Yampa
import FRP.Yampa.InternalCore as Yampa
import Data.IORef
import Debug.Trace

width  = 640
height = 480

main = do
  timeRef <- newIORef (0 :: Int)
  controllerRef <- newIORef $ Controller False False False False False -- (Controller Nothing)
  reactimate (initGraphs >> readIORef controllerRef)
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                mInput <- sdlGetController controllerRef
                let dt = if (controllerFire mInput)
                            then -dtSecs
                            else dtSecs
                -- print (mInput)
                return (dt, Just mInput)
             )
             (\_ e -> display ((0,0),e) >> return False)
             (player initialPos)

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

-- We need a non-blocking controller-polling function.
sdlGetController :: IORef Controller -> IO Controller
sdlGetController controllerState = do
  state <- readIORef controllerState
  e     <- pollEvent
  case e of
    KeyDown v -> updateControllerState controllerState v True  >> sdlGetController controllerState
    KeyUp   v -> updateControllerState controllerState v False >> sdlGetController controllerState
    _         -> return state

updateControllerState :: IORef Controller -> Keysym -> Bool -> IO ()
updateControllerState stateRef input state = modifyIORef stateRef (applyInput input state)

applyInput :: Keysym -> Bool -> Controller -> Controller
applyInput input apply c = keyF input apply
 where keyF (Keysym SDLK_w _ _) s = c { controllerUp    = s }
       keyF (Keysym SDLK_a _ _) s = c { controllerLeft  = s }
       keyF (Keysym SDLK_s _ _) s = c { controllerDown  = s }
       keyF (Keysym SDLK_d _ _) s = c { controllerRight = s }
       keyF (Keysym SDLK_k _ _) s = c { controllerFire = s }
       keyF _ _ = c


data Controller = Controller
 { controllerUp    :: Bool
 , controllerDown  :: Bool
 , controllerLeft  :: Bool
 , controllerRight :: Bool
 , controllerFire  :: Bool
 }

getVelocity :: Controller -> (Double, Double)
getVelocity rc = (rx, ry)
 where ry = rup + rdown
       rx = rleft + rright
       rup    = if controllerUp rc    then -1 else 0
       rdown  = if controllerDown rc  then 1  else 0
       rleft  = if controllerLeft rc  then -1 else 0
       rright = if controllerRight rc then 1  else 0

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

display :: ((Double, Double),(Double, Double)) -> IO()
display ((boxY,_), (playerX, playerY)) = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  green <- mapRGB format 0 0xFF 0
  fillRect screen Nothing green

  -- Paint small red square, at an angle 'angle' with respect to the center
  red <- mapRGB format 0xFF 0 0
  let side = 10
      x = (width - side) `div` 2
      y = round boxY
  fillRect screen (Just (Rect x y side side)) red

  drawPlayer screen (round playerX, round playerY)

  -- Double buffering
  SDL.flip screen

drawPlayer surface (playerX, playerY) = do
  let format = surfaceGetPixelFormat surface
  red <- mapRGB format 0xFF 0 0
  let side = 10
      x = playerX
      y = playerY
  fillRect surface (Just (Rect x y side side)) red

-- |  Moves by default using playerProgress, unless the player is
-- slowing down and barely moving, in which case we discard any
-- remanent velocity and reinitiate the player from the new position.
player :: (Double, Double) -> SF (Controller) (Double, Double)
player p0 = revSwitch (playerProgress p0 >>> (arr fst &&& isTooSmall)) falling
 where isTooSmall :: SF ((Double, Double), (Double, Double)) (Yampa.Event (Double, Double))
       isTooSmall = proc (pos,diffV) -> do
           derivV <- derivative -< diffV
           returnA -< if shouldStop diffV derivV
                         then (Yampa.Event pos)
                         else Yampa.NoEvent

falling :: (Double, Double) -> SF Controller (Double, Double)
falling p0 = proc (_) -> do
  v <- integral -< (0, -9.8)
  p <- (p0 ^+^) ^<< integral -< v
  returnA -< trace (show p) p

shouldStop :: (Double, Double) -> (Double, Double) -> Bool
shouldStop (dx, dy) (diffVX, diffVY) =
  abs dx < margin && abs dy < margin                -- Small movement
  && sign dx * diffVX <= 0 && diffVY * sign dy <= 0 -- Slowing down
  && (abs dy > 0 || abs dx > 0)                     -- But moving (break in switch loop)
 where margin = 0.1

playerProgress :: (Double, Double) -> SF Controller ((Double, Double), (Double, Double))
playerProgress p0 = proc (c) -> do
  rec let acc = getVelocity c               -- Acceleration (depends on user input)
      v     <- integral -< acc              -- Velocity according to user input
      vdiff <- integral -< 0.1 *^ vtotal    -- "Air" resistance (note: I get strange flickers with exponentiation)
      let vtotal = v ^-^ vdiff              -- Subtract resistance from velocity (FIXME: make sure we don't move back")
      p <- (p0 ^+^) ^<< integral -< vtotal  -- Add to initial position
  returnA -< (p, vtotal)

-- | FIXME: the old code (commented out) contains a "bug" that makes
-- it misbehave when you change direction twice without stopping.
-- But this code does not guarantee that resistance is not greater than
-- current velocity, which means that (theoretically) it could drag the
-- player back even if it's stopped. I don't know if that can happen
-- because I don't understand how rec works above (but I wrote the thing ;)
-- applyResistance :: (Double, Double) -> (Double, Double) -> (Double, Double)
-- applyResistance (vx, vy) (vdx, vdy) = (vx', vy')
--  where vx' = if abs vdx > abs vx then 0 else (vx - vdx)
--        vy' = if abs vdy > abs vy then 0 else (vy - vdy)

sign :: Double -> Double
sign d | d < 0     = -1
       | otherwise = 1

initialPos = ((fromIntegral width/2), (fromIntegral height/2))

-- | Reversible switch.
--
-- By default, the first signal function is applied.
--
-- Whenever the second value in the pair actually is an event,
-- the value carried by the event is used to obtain a new signal
-- function to be applied *at that time and at future times*.
--
-- Until that happens, the first value in the pair is produced
-- in the output signal.
--
-- Important note: at the time of switching, the second
-- signal function is applied immediately. If that second
-- SF can also switch at time zero, then a double (nested)
-- switch might take place. If the second SF refers to the
-- first one, the switch might take place infinitely many
-- times and never be resolved.
--
-- Remember: The continuation is evaluated strictly at the time
-- of switching!
revSwitch :: SF a (b, Yampa.Event c) -> (c -> SF a b) -> SF a b
revSwitch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
            case tf10 a0 of
                (sf1, (b0, Yampa.NoEvent))  -> (switchAux sf1 k, b0)
                (sf1, (_,  Yampa.Event c0)) -> switchingPoint sf1 k (sfTF (k c0) a0)

        switchingPoint :: SF' a (b, Yampa.Event c) -> (c -> SF a b) -> (SF' a b, b) -> (SF' a b, b)
        switchingPoint sf1 k (sfN', b) = (sf', b)
          where sf' = SF' tf'
                tf' dt a = if | dt < 0  -> sfTF' (switchAux sf1 k) dt a
                                           -- let (sf1', b') = sfTF' sf1 dt a
                                           -- in (switchAux sf1' k, b')
                              | dt > 0  -> switchingPoint' sf1 k dt (sfTF' sfN' dt a)
                              | dt == 0 -> switchingPoint sf1 k (sfN', b)

        switchingPoint' :: SF' a (b, Yampa.Event c) -> (c -> SF a b) -> DTime -> (SF' a b, b) -> (SF' a b, b)
        switchingPoint' sf1 k accumDT (sfN', b) = (sf', b)
          where sf' = SF' tf'
                tf' dt a = let dt' = dt + accumDT
                           in if | dt < 0  -> if | dt' < 0  -> sfTF' (switchAux sf1 k) dt' a
                                                 | dt' > 0  -> dt' `seq` switchingPoint' sf1 k dt' (sfTF' sfN' dt a)
                                                 | dt' == 0 -> switchingPoint' sf1 k accumDT (sfN', b)
                                 | dt > 0  -> dt' `seq` switchingPoint' sf1 k dt' (sfTF' sfN' dt a)
                                 | dt == 0 -> switchingPoint' sf1 k accumDT (sfN', b)


        switchAux :: SF' a (b, Yampa.Event c) -> (c -> SF a b) -> SF' a b
        switchAux sf1                          k = SF' tf
            where
                tf dt a =
                    case (sfTF' sf1) dt a of
                        (sf1', (b, Yampa.NoEvent)) -> (switchAux sf1' k, b)
                        (_,    (_, Yampa.Event c)) -> switchingPoint sf1 k (sfTF (k c) a)

