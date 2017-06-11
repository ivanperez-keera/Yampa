{-# LANGUAGE Arrows #-}
import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.IORef
import Data.List
import Data.Maybe
import Debug.Trace
import FRP.Yampa           as Yampa
import FRP.Yampa.Utilities (parZ)
import GHC.Float
import Graphics.UI.SDL     as SDL
import Graphics.UI.SDL.TTF as TTF
import System.CWiid

import IdentityList

data Resources = Resources
  { resFont         :: Font
  }

main = do
  timeRef <- newIORef (0 :: Int)
  _       <- yampaSDLTimeSense timeRef
  _       <- yampaSDLTimeSense timeRef
  _       <- yampaSDLTimeSense timeRef
  _       <- yampaSDLTimeSense timeRef

  -- Initialise SDL
  SDL.init [InitVideo]

  wiimote <- initializeWiimote

  res     <- loadResources

  when (isJust res) $ do
    let res'     = fromJust res
        wiimote' = fromJust wiimote
    controllerRef <- newIORef $ Controller (0,0) False False
    reactimate (initGraphs >> yampaSDLTimeSense timeRef >> readIORef controllerRef)
               (\_ -> do
                  dtSecs <- yampaSDLTimeSense timeRef

                  -- Input sensing
                  -- Wiimote is optional and takes preference
                  mInput <- if isJust wiimote
                             then senseWiimote wiimote' controllerRef
                             else sdlGetController controllerRef

                  return (dtSecs, Just mInput)
               )
               (\_ e -> display res' e >> threadDelay 10 >> return False)
               (game 0)

initializeWiimote = do 
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  maybe (return ()) (\wm' -> cwiidSetRptMode wm' >> return ()) wm
  return wm

senseWiimote wiimote controllerRef = do
  senseLeft wiimote controllerRef
  senseRight wiimote controllerRef
  senseClick wiimote controllerRef
  readIORef controllerRef
 
senseLeft wiimote controllerRef = do
  controller <- readIORef controllerRef
  flags <- cwiidGetBtnState wiimote
  let isLeft = cwiidIsBtnEnabled flags cwiidBtnLeft
      (x,y)  = controllerPos controller
      x'     = if isLeft then x - wiiXDiff else x
      x''    = if x' < 0 then 0 else x'
  writeIORef controllerRef (controller { controllerPos = (x'', y) })
 
senseRight wiimote controllerRef = do
  controller <- readIORef controllerRef
  flags <- cwiidGetBtnState wiimote
  let isRight = cwiidIsBtnEnabled flags cwiidBtnRight
      (x,y)  = controllerPos controller
      x'     = if isRight then x + wiiXDiff else x
      x''    = if x' > gameWidth then gameWidth else x'
  writeIORef controllerRef (controller { controllerPos = (x'', y) })

senseClick wiimote controllerRef = do
  controller <- readIORef controllerRef
  flags <- cwiidGetBtnState wiimote
  let isClick = cwiidIsBtnEnabled flags cwiidBtnA
      controller' = controller { controllerClick = isClick }
  writeIORef controllerRef controller'

loadResources :: IO (Maybe Resources)
loadResources = do
  -- Font initialization
  ttfOk <- TTF.init
  
  -- Ugly code. Some monad transformer must be able to help me.
  if ttfOk
   then do font <- TTF.tryOpenFont "lacuna.ttf" 32 -- What does the 32 do?
           if isNothing font
             then do putStrLn "Font cannot be opened"
                     return Nothing
             else return $ Just $ Resources (fromJust font)
   else return Nothing

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
  let dtSecs = fromIntegral dt / 1000
  return dtSecs

-- We need a non-blocking controller-polling function.
sdlGetController :: IORef Controller -> IO Controller
sdlGetController = sdlGetController'

sdlGetController' :: IORef Controller -> IO Controller
sdlGetController' controllerState = do
  e <- pollEvent
  case e of
    MouseMotion x y _ _ -> do modifyIORef controllerState (\c -> c { controllerPos = (fromIntegral x,fromIntegral y)}) 
                              sdlGetController' controllerState
    MouseButtonDown _ _ ButtonLeft -> do modifyIORef controllerState (\c -> c { controllerClick = True }) 
                                         sdlGetController' controllerState
    MouseButtonUp   _ _ ButtonLeft -> do modifyIORef controllerState (\c -> c { controllerClick = False}) 
                                         sdlGetController' controllerState
    KeyUp (Keysym { symKey = SDLK_f }) -> do modifyIORef controllerState (\c -> c { controllerPause = not (controllerPause c) }) 
                                             sdlGetController' controllerState
    _                   -> readIORef controllerState

data Controller = Controller
 { controllerPos   :: (Double, Double)
 , controllerClick :: Bool
 , controllerPause :: Bool
 }

initGraphs :: IO ()
initGraphs = do
  -- Create window
  screen <- SDL.setVideoMode (round width) (round height) 16 [SWSurface]
  SDL.setCaption "Test" ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

display :: Resources -> GameState -> IO()
display resources shownState = do 
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  green <- mapRGB format 0 0xFF 0
  fillRect screen Nothing green

  hud <- createRGBSurface [SWSurface]
             (round width) (round gameTop)
             32 0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
  paintGeneral hud resources (showGeneral shownState)
  let rectHud = SDL.Rect 0 0 (round width) (round gameTop)
  SDL.blitSurface hud Nothing screen $ Just rectHud

  -- The following line is BIG_ENDIAN specific
  -- The 32 is important because we are using Word32 for the masks
  -- FIXME: Should I use HWSurface and possibly other flags (alpha?)?
  surface <- createRGBSurface [SWSurface]
             (round gameWidth) (round gameHeight)
             32 0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
  paintGeneralMsg surface resources (gameStatus (showGeneral shownState))
  mapM_ (paintObject surface) $ showObjects shownState
  let rect = SDL.Rect (round gameLeft) (round gameTop) (round gameWidth) (round gameHeight)
  SDL.blitSurface surface Nothing screen $ Just rect

  -- Double buffering
  SDL.flip screen

paintGeneral screen resources over = void $ do
  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format 0x11 0x22 0x33
  fillRect screen Nothing bgColor
  paintGeneralHUD screen resources over

paintGeneralMsg screen resources GamePlaying     = return ()
paintGeneralMsg screen resources GamePaused      = paintGeneralMsg' screen resources "Paused"
paintGeneralMsg screen resources (GameLoading n) = paintGeneralMsg' screen resources ("Level " ++ show n)

paintGeneralMsg' screen resources msg = void $ do
  let font = resFont resources
  message <- TTF.renderTextSolid font msg (SDL.Color 128 128 128)
  let x = (SDL.surfaceGetWidth  screen - w) `div` 2
      y = (SDL.surfaceGetHeight screen - h) `div` 2
      w = SDL.surfaceGetWidth  message
      h = SDL.surfaceGetHeight message
  SDL.blitSurface message Nothing screen $ Just (SDL.Rect x y w h)

paintGeneralHUD screen resources over = void $ do
  let font = resFont resources
  message1 <- TTF.renderTextSolid font ("Level: " ++ show (level over)) (SDL.Color 128 128 128)
  let w1 = SDL.surfaceGetWidth  message1
      h1 = SDL.surfaceGetHeight message1
  SDL.blitSurface message1 Nothing screen $ Just (SDL.Rect 10 10 w1 h1)
  message2 <- TTF.renderTextSolid font ("Points: " ++ show (points over)) (SDL.Color 128 128 128)
  let w2 = SDL.surfaceGetWidth  message2
      h2 = SDL.surfaceGetHeight message2
  SDL.blitSurface message2 Nothing screen $ Just (SDL.Rect 10 (10 + h1 + 5) w2 h2)

paintObject screen object = do
  red <- mapRGB format 0xFF 0 0
  case objectKind object of
    (Paddle (w,h)) -> void $ fillRect screen (Just (Rect x y (round w) (round h))) red
    (Block (w,h))  -> void $ fillRect screen (Just (Rect x y (round w) (round h))) red
    (Ball r)       -> void $ do let x' = x - round r
                                    y' = y - round r
                                    sz = round (2*r)
                                fillRect screen (Just (Rect x' y' sz sz)) red
    _              -> return ()
  where format = surfaceGetPixelFormat screen
        p      = objectPos object
        x      = round (fst p)
        y      = round (snd p)

data ObjectKind = Ball    Double -- radius?
                | Paddle  Size2D 
                | Block   Size2D
                | Side    Side
 deriving (Show,Eq)

isBlock :: ObjectKind -> Bool
isBlock (Block _) = True
isBlock _         = False

type Size2D = (Double, Double)
type Pos2D  = (Double, Double)
type Vel2D  = (Double, Double)
type Acc2D  = (Double, Double)

data Side = TopSide | BottomSide | LeftSide | RightSide
 deriving (Eq,Show)

oppositeSide :: Side -> Side
oppositeSide TopSide    = BottomSide
oppositeSide BottomSide = TopSide
oppositeSide LeftSide   = RightSide
oppositeSide RightSide  = LeftSide

type ObjectSF = SF ObjectInput ObjectOutput

data ObjectInput = ObjectInput
 { userInput    :: Controller
 , collisions   :: Collisions
 , knownObjects :: Objects
 }

data ObjectOutput = ObjectOutput
 { outputObject :: Object
 , harakiri     :: Yampa.Event ()
 } 

data Object = Object { objectName           :: String
                     , objectKind           :: ObjectKind
                     , objectPos            :: Pos2D
                     , objectVel            :: Vel2D
                     , objectAcc            :: Acc2D
                     , objectDead           :: Bool
                     , canCauseCollisions   :: Bool
                     , collisionEnergy      :: Double
                     , displacedOnCollision :: Bool       -- Theoretically, setting cE == 0 should suffice
                     }
 deriving (Show)

isPaddle :: Object -> Bool
isPaddle o = case objectKind o of
  (Paddle _) -> True
  _          -> False

type ObjectSFs = IL ObjectSF
type Objects   = [Object]
type ObjectOutputs = [ObjectOutput]

data GameState = GameState
  { showObjects :: Objects
  , showGeneral    :: General
  }

data General = General
  { gameStatus :: GameStatus
  , lives      :: Int
  , level      :: Int
  , points     :: Int
  }

data GameStatus = GamePlaying
                | GamePaused
                | GameLoading Int

type Collisions = [Collision]

data Collision = Collision { collisionData :: [(String, Vel2D)] } -- ObjectId x Velocity
 deriving Show


-- Simple game loop architectures

-- Case #1
-- camera >>> scene >>> (render >>> swap)

-- Case #2 (pong-like game)
-- readInput >>> (if not quit)
--           >>> movePaddles                                                 -- scene 1.1
--           >>> moveBall                                                    -- scene 1.2
--           >>> collideAndBounceBall                                        -- scene 1.3
--           >>> (when leftImpact  >>> (increaseScoreRight *** resetBall))   -- scene 1.4
--           >>> (when rightImpact >>> (increaseScoreLeft  *** resetBall))   -- scene 1.5
--           >>> render

-- Case #3 (event-based, message-passing-based)

-- Candidate #1
--
-- readInput >>> arrowLoop ((instantLoop (update Physics >>> solve Collisions))
--                          >>> updateLogic)
--           >>> render
--
-- In this case, the internal pipe (inside arrowloop) behaves like a state
-- transforming function, with the whole loop just being a state monad.
-- The first part of the loop (the instantLoop) would have to concatenate and
-- propagate collisions, so that the remained of the game can act upon them.

-- Is there any difference between this and pure, state-based, imperative
-- programming? (apart from the fact that it has no side effects inside the
-- loop itself)

-- Questions: Can the physics and collision detection system provide
-- one of the following:
-- * Suggestions about when to sample:
--    * This would not guarantee sampling at the time of impact,
--      as such time could be impossible to realize with computer
--      precision: sample for time t could be "too soon", and
--      sample for time t + minimum_delta could be "too late".
-- * Perfect POI and TOI?
--    * The degree of perfection would depend on imperfect things
--      (such as the imprecise euler-based integration function),
--      but it might still be perfectly precise as far as Haskell
--      is concerned.

game :: Int -> SF Controller GameState
game level = dSwitch (gamePlay level >>> (arr id &&& isLevelOver))
                     (\g -> let level' = (level+1) `mod` numLevels
                                pts    = gamePoints g
                            in loadLevel level' loadingDelay (game level') >>> arr (addPoints pts))

gamePoints :: GameState -> Int
gamePoints = points . showGeneral

addPoints :: Int -> GameState -> GameState
addPoints n g = g { showGeneral = gen { points = ov + n } }
 where gen = showGeneral g
       ov  = points gen

gamePlay :: Int -> SF Controller GameState
gamePlay level =
  ((arr id) &&& (pause undefined (False --> isPaused) (mainLoop level)))
  >>> pauseGeneral

loadLevel :: Int -> DTime -> SF a GameState -> SF a GameState 
loadLevel level time next =
  switch (levelGeneral level &&& after time ()) (\_ -> next)

isLevelOver :: SF GameState (Yampa.Event GameState)
isLevelOver = proc (s) -> do
  over <- edge -< not $ any isBlock (map objectKind (showObjects s))
  let snapshot = over `tag` s
  returnA -< snapshot

levelGeneral :: Int -> SF a GameState
levelGeneral n =
  let defGeneral = General { gameStatus = GameLoading n -- (Just $ "Level " ++ show n)
                     , level  = n
                     , lives  = 0
                     , points = 0
                     }
  in  arr $ const $ GameState { showGeneral = defGeneral , showObjects = [] }

addLevel :: Int -> SF GameState GameState
addLevel l = arr $ \g -> let o = showGeneral g in g { showGeneral = o { level = l } }

pauseGeneral :: SF (Controller, GameState) GameState
pauseGeneral = proc (c, g) -> do
  let isPause = controllerPause c
  let o       = showGeneral g
  returnA -< if isPause
                then g { showGeneral = o { gameStatus = GamePaused } }
                else g

isPaused :: SF Controller Bool
isPaused = arr controllerPause

mainLoop :: Int -> SF Controller GameState -- Objects
mainLoop level = mainLoop' (initialObjects level) >>> (extractObjects *** arr id) >>> addGeneral level

addGeneral :: Int -> SF (Objects,Int) GameState
addGeneral level = arr (\(objs,points) -> GameState objs (General GamePlaying 0 level points))

mainLoop' :: ObjectSFs -> SF Controller (ObjectOutputs,Int)
mainLoop' objs = loopPre ([],[],0) $
   ((adaptInput >>> processMovement >>> (arr elemsIL &&& detectCollisions'))
   &&& (arr (trd3.snd)))
   >>> (arr fst &&& arr (\((_,cs),o) -> o + countPoints cs))        -- Adds the old point count to
                                                                    -- the newly-made points
   >>> (arr (\(x,y) -> (fst x, y)) &&& arr (\((x,y),z) -> (x,y,z))) -- Select (objects+points, objects+collisions+points)

 where -- Just reorder the input
       adaptInput :: SF (Controller, (ObjectOutputs, Collisions, Int)) ObjectInput
       adaptInput = arr (\(gi,(os,cs,pts)) -> ObjectInput gi cs (map outputObject os))

       -- Parallely apply all object functions
       processMovement :: SF ObjectInput (IL ObjectOutput)
       processMovement = processMovement' objs

       processMovement' :: ObjectSFs -> SF ObjectInput (IL ObjectOutput)
       processMovement' objs = -- parB objs
                         dpSwitch 
                           route
                           objs
                           (noEvent --> arr suicidalSect)
                           (\sfs' f -> processMovement' (f sfs'))

       -- Apply the input to every object signal transformer
       route :: ObjectInput -> IL sf -> IL (ObjectInput, sf)
       route k l = mapIL (\(_,o) -> (k, o)) l

       suicidalSect :: (a, IL ObjectOutput) -> (Yampa.Event (IL ObjectSF -> IL ObjectSF))
       suicidalSect (_,oos) =
         -- Turn every event carrying a function that transforms the
         -- object signal function list into one function that performs
         -- all the efects in sequence
         foldl (mergeBy (.)) noEvent es

         -- Turn every object that wants to kill itself into
         -- a function that removes it from the list
         where es :: [Yampa.Event (IL ObjectSF -> IL ObjectSF)]
               es = [ (harakiri oo `tag` (deleteIL k))
                    | (k,oo) <- assocsIL oos ]

       -- From the actual objects, detect which ones collide
       detectCollisions' :: SF (IL ObjectOutput) Collisions
       detectCollisions' = extractObjects >>> detectCollisions

       -- Count-points
       countPoints :: Collisions -> Int
       countPoints = (sum . map numPoints)
         where numPoints (Collision cd)
                  | hasBall cd = countBlocks cd
                  | otherwise  = 0
               hasBall     = any ((=="ball").fst)
               countBlocks = length . filter ((isPrefixOf "block").fst)

extractObjects :: Functor f => SF (f ObjectOutput) (f Object)
extractObjects = arr (fmap outputObject)

initialObjects :: Int -> ObjectSFs
initialObjects level = listToIL $ [ objPaddle,   objSideRight,  objSideTop
                                  , objSideLeft, objSideBottom, objBall
                                  ]
                            ++ map (\p -> objBlockAt p (blockWidth, blockHeight)) (blockPosS level)


-- How does one object depend on the state of other objects, or update itself
-- based on them?
-- The ball behaves like a normal ball until it touches the bottom, then
-- goes on again and again.
--
-- Define a new rSwitch in Yampa for this.
objBall :: ObjectSF
objBall = switch (normalBehavior &&& collidedWithBottom)
                 (\_ -> objBall)

  where -- Normal Behaviour is: follow the paddle around and, upon release,
        -- become a good-old bouncing ball with a certain initial velocity,
        -- starting from the point where the ball was when it was released.
        normalBehavior = switch ((arr id &&& followPaddle) >>> (arr snd &&& release'))
                                (\p -> bouncingBall p initialBallVel)

release' :: SF (ObjectInput, ObjectOutput) (Yampa.Event Pos2D)
release' = proc (ObjectInput gi cs os, o) -> do

   -- Do we have a click? If so, fire an event.
   -- justClick :: Yampa.Event ()
   let isClick = controllerClick gi
   justClick <- edge -< isClick

   -- When there really is a click, insert the position in the event
   let curPos = objectPos $ outputObject o
       result = tag justClick curPos

   returnA -< result

collidedWithBottom :: SF ObjectInput (Yampa.Event ())
collidedWithBottom = proc (ObjectInput _ cs _) -> do
  let ballAndBottomCollisions                = filter ballAndBottomCollision cs
      ballAndBottomCollision (Collision cs') = any ballCollision cs' && any bottomCollision cs'
      ballCollision   = (== "ball").fst       -- Ugly, ugly, ugly
      bottomCollision = (== "bottomWall").fst -- Ugly, ugly, ugly
  justBallAndBottomCollision <- edge -< not (null ballAndBottomCollisions)
  returnA -< justBallAndBottomCollision

-- Ball follows the paddle if there is one, and it's out of the screen
-- otherwise). To avoid reacting to collisions, this ball is non-interactive.
followPaddle :: ObjectSF
followPaddle = arr $ \(ObjectInput _ _ os) ->
  let paddlePos = fmap objectPos $ find isPaddle os
      ballPos   = maybe (outOfScreen, outOfScreen) ((ballWidth, - ballHeight) ^+^) paddlePos
  in ObjectOutput (Object { objectName           = "ball"
                          , objectKind           = Ball ballWidth
                          , objectPos            = ballPos
                          , objectVel            = (0, 0)
                          , objectAcc            = (0, 0)
                          , objectDead           = False
                          , canCauseCollisions   = False
                          , collisionEnergy      = 0
                          , displacedOnCollision = False
                          })
                  noEvent
                  
            
 where outOfScreen = (-10) -- Ugly, ugly, ugly

-- A bouncing ball moves freely until there is a collition, then bounces and
-- goes on and on.
--
-- This SF needs an initial position and velocity, of the
-- point of collision and new velocity after a collision.
bouncingBall p0 v0 =
  switch progressAndBounce
         (uncurry bouncingBall) -- Somehow it's clearer like this:
                                -- \(p', v') -> bouncingBall p' v')

 where
       -- The ballBounce needs the input (which has knowledge of collisions),
       -- so we carry it parallely to the tentative positions, and then
       -- use it to detect when it's time to bounce
       progressAndBounce = (arr id &&& freeBall') >>> (arr snd &&& ballBounce)
       freeBall'         = discardInput >>> freeBall p0 v0
       discardInput      = arr (const ())

--      ==========================    ============================
--     -==--------------------->==--->==-   ------------------->==
--    / ==                      ==    == \ /                    ==
--  --  ==                      ==    ==  X                     ==
--    \ ==                      ==    == / \                    ==
--     -==----> freeBall' ----->==--->==--------> ballBounce -->==
--      ==========================    ============================
       

ballBounce = noEvent --> ballBounce'

ballBounce' = proc (ObjectInput ci cs os, o) -> do
  let ballC = concatMap (filter ((== "ball").fst) . collisionData) cs
      p'    = objectPos $ outputObject o      -- Should we pass the p in ballC??? Sounds more reasonable.
      v'    = snd (head ballC) -- Only defined if ballC is not null
  let e     = if null ballC then noEvent else Event (p',v')
  returnA -< e

freeBall p0 v0 = proc () -> do
  p <- (p0 ^+^) ^<< integral -< v0
  let obj = Object { objectName           = "ball"
                   , objectKind           = Ball ballWidth
                   , objectPos            = p
                   , objectVel            = v0
                   , objectAcc            = (0, 0)
                   , objectDead           = False
                   , canCauseCollisions   = True
                   , collisionEnergy      = 1
                   , displacedOnCollision = True
                   }
  
  returnA -< livingObject obj

objPaddle = proc (ObjectInput ci cs os) -> do
  p <- player     -< ci
  v <- derivative -< p
  returnA -< livingObject $
               Object{ objectName           = "paddle"
                   , objectKind           = Paddle (paddleWidth,paddleHeight)
                   , objectPos            = p
                   , objectVel            = v
                   , objectAcc            = (0,0)
                   , objectDead           = False
                   , canCauseCollisions   = True
                   , collisionEnergy      = 0
                   , displacedOnCollision = False
                   }

objBlockAt (x,y) (w,h) = proc (ObjectInput ci cs os) -> do
  let name   = "blockat" ++ show (x,y)
      isDead = not $ null $ concatMap (filter ((== name).fst) . collisionData) cs
  dead <- edge -< isDead
  returnA -< ObjectOutput (Object{ objectName           = name
                                 , objectKind           = Block (w, h)
                                 , objectPos            = (x,y)
                                 , objectVel            = (0,0)
                                 , objectAcc            = (0,0)
                                 , objectDead           = isDead
                                 , canCauseCollisions   = False
                                 , collisionEnergy      = 0
                                 , displacedOnCollision = False
                                 })
                           dead

livingObject o = ObjectOutput o noEvent

player :: SF (Controller) (Double, Double)
player = proc c -> do
  let p   = controllerPos c
      x'  = fst p - (paddleWidth/2)
      x'' = inRange (0, gameWidth-paddleWidth) x'
  returnA -< (x'', gameHeight - paddleMargin)

objSideRight = arr $ const $  livingObject $
               Object { objectName = "rightWall"
                      , objectKind           = Side RightSide
                      , objectPos            = (gameWidth,0) 
                      , objectVel            = (0,0)
                      , objectAcc            = (0,0)
                      , objectDead           = False
                      , canCauseCollisions   = False
                      , collisionEnergy      = 0
                      , displacedOnCollision = False
                      }

objSideLeft = arr $ const $  livingObject $
               Object { objectName = "leftWall"
                      , objectKind           = Side LeftSide
                      , objectPos            = (0,0) 
                      , objectVel            = (0,0)
                      , objectAcc            = (0,0)
                      , objectDead           = False
                      , canCauseCollisions   = False
                      , collisionEnergy      = 0
                      , displacedOnCollision = False
                      }

objSideTop = arr $ const $  livingObject $
               Object { objectName  = "topWall"
                      , objectKind           = Side TopSide
                      , objectPos            = (0,0) 
                      , objectVel            = (0,0)
                      , objectAcc            = (0,0)
                      , objectDead           = False
                      , canCauseCollisions   = False
                      , collisionEnergy      = 0
                      , displacedOnCollision = False
                      }

objSideBottom = arr $ const $  livingObject $
               Object { objectName = "bottomWall"
                      , objectKind           = Side BottomSide
                      , objectPos            = (0, gameHeight) 
                      , objectVel            = (0,0)
                      , objectAcc            = (0,0)
                      , objectDead           = False
                      , canCauseCollisions   = False
                      , collisionEnergy      = 0
                      , displacedOnCollision = False
                      }

updateObjPos :: SF (ILKey, Object) (ILKey, Object)
updateObjPos = proc (i,o) -> do
  -- Since we are saving the position to avoid having to keep the last known
  -- position in memory every time and integrate over a range every time
  -- (would that really happen???) we use an integral over an interval.
  -- I really wonder if this integration thing in Yampa works the way it is
  -- expected to work. Does it work well for non-linear equations?
  --
  -- Integral only for dt interval
  actualVel <- iterFrom (\_ (v1,v2) dt _ -> (v1 * dt, v2 * dt)) (0,0) -< objectVel o

  -- Update position
  let newPos = objectPos o ^+^ actualVel
      o'     = o { objectPos = newPos }
  returnA -< (i,o')

detectCollisions :: SF (IL Object) Collisions
detectCollisions = proc objsT -> do
  let (moving, static) = partition (canCauseCollisions.snd) $ assocsIL objsT
      collisions       = detectCollisions' objsT moving
      flattened        = filter (\(Collision n) -> not (null n)) collisions
  returnA -< flattened

detectCollisions'  objsT ms = concatMap (detectCollisions''  objsT) ms
detectCollisions'' objsT m  = concatMap (detectCollisions''' m) (assocsIL objsT)
detectCollisions''' m o
 | fst m == fst o = []
 | otherwise      = maybeToList (detectCollision m o)

-- Detects a collision between one object and another
-- regardless of everything else
-- FIXME: should we use the last known positions? Or should
-- velocities suffice?
detectCollision :: (ILKey, Object) -> (ILKey, Object) -> Maybe Collision
detectCollision obj1 obj2
  | overlap obj1 obj2 = let r = Just $ collisionResponse obj1 obj2 in r
  | otherwise         = Nothing

overlap obj1 obj2 = overlapShape (objShape $ snd obj1) (objShape $ snd obj2)

objShape :: Object -> Shape
objShape obj = case objectKind obj of
  (Ball r)   -> Rectangle (p ^-^ (r,r)) (2*r, 2*r)
  (Paddle s) -> Rectangle p s
  (Block  s) -> Rectangle p s
  (Side   s) -> sideToShape p s
 where p = objectPos obj
       width'  = gameWidth
       height' = gameHeight
       d = collisionErrorMargin
       sideToShape p TopSide    = Rectangle (p ^-^ (d, d)) (width' + 2*d, d)
       sideToShape p LeftSide   = Rectangle (p ^-^ (d, d)) (d, height' + 2*d)
       sideToShape p RightSide  = Rectangle (p ^-^ (0, d)) (d, height' + 2*d)
       sideToShape p BottomSide = Rectangle (p ^-^ (d, 0)) (width' + 2*d, d)

overlapShape :: Shape -> Shape -> Bool
overlapShape (Rectangle p1 s1) (Rectangle p2 s2) = abs dx <= w && abs dy <= h
  where (dx,dy) = (p1 ^+^ (0.5 *^ s1)) ^-^ (p2 ^+^ (0.5 *^ s2))
        (w,h)   = 0.5 *^ (s1 ^+^ s2)

collisionResponse :: (ILKey, Object) -> (ILKey, Object) -> Collision
collisionResponse o1 o2 =
  Collision $
    map objectToCollision [(o1, side), (o2, side')]
    -- mapFilter objectToCollision (objectReacts.fst) [(o1, side), (o2, side')]
  where side  = collisionSide (snd o1) (snd o2)
        side' = oppositeSide side
        objectReacts      o             = collisionEnergy o > 0 || displacedOnCollision o
        objectToCollision ((oid, o),s)  = (objectName o, correctVel (objectVel o) (collisionEnergy o) s)
        correctVel (vx,vy) e TopSide    = (vx, ensurePos (vy * (-e)))
        correctVel (vx,vy) e BottomSide = (vx, ensureNeg (vy * (-e)))
        correctVel (vx,vy) e LeftSide   = (ensureNeg (vx * (-e)),vy)
        correctVel (vx,vy) e RightSide  = (ensurePos (vx * (-e)),vy)

collisionSide :: Object -> Object -> Side
collisionSide obj1 obj2 = collisionSide' (objShape obj1) (objShape obj2)

collisionSide' :: Shape -> Shape -> Side
collisionSide' (Rectangle p1 s1) (Rectangle p2 s2)
   | wy > hx && wy > -hx = TopSide
   | wy > hx             = LeftSide
   | wy > -hx            = RightSide
   | otherwise           = BottomSide
  where (dx,dy) = (p1 ^+^ (0.5 *^ s1)) ^-^ (p2 ^+^ (0.5 *^ s2)) -- p1 ^-^ p2
        (w,h)   = 0.5 *^ (s1 ^+^ s2)
        wy      = w * dy
        hx      = h * dx

data Shape = -- Circle    Pos2D Float   -- Position and radius -- NOT FOR NOW
             -- |
             Rectangle Pos2D Size2D  -- Position and size
             -- | SemiPlane Pos2D Float   -- Position and angle of plane normal
      
paddleWidth  = 200
paddleHeight = 10
paddleMargin = 50
ballWidth    = 10
ballHeight   = 10
ballMargin   = 30
blockWidth   = 80
blockHeight  = 20

initialBallVel = (300, -300)

numLevels    = 2
loadingDelay = 2 -- seconds

collisionErrorMargin :: Double
collisionErrorMargin = 100

wiiXDiff = 2

width :: Double
width  = 640
height :: Double
height = 600

gameTop    = 100
gameHeight = height - gameTop
gameWidth  = width
gameLeft   = 0

-- Levels
-- Level 0
blockRows    = 1
blockColumns = (-2) + gameWidth / blockWidth
blockHPos    = map (blockWidth *) [0..blockColumns-1]
blockVPos    = map (100+) $ map (blockHeight*) [1..blockRows]
blockPosS 0  = [(x,y) | x <- blockHPos, y <- blockVPos]
-- Level 1
blockPosS 1  = map (\(x,y) -> (x,100+y)) $ map (\(x,y) -> (blockWidth*x, blockHeight*y))
               [(x,y) | x <- [0..1], y <- [0..2], x /= y]

-- Auxiliary Yampa stuff

-- holdWhen behaves normally, outputting only the b, when the second value
-- is false, and it holds the last known value when the value is True. 
holdWhen :: b -> SF a (b,Bool) -> SF a b
holdWhen b_init sf = sf >>> holdOutput >>> hold b_init
 where holdOutput = arr (\(b,discard) -> if discard then noEvent else Yampa.Event b)

trd3 :: (a,b,c) -> c
trd3 (a,b,c) = c

mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p = map f . filter p 

inRange :: Ord a => (a,a) -> a -> a
inRange (mN, mX) x = min mX (max mN x)

ensurePos :: (Eq a, Num a) => a -> a
ensurePos e = if signum e == (-1) then negate e else e

ensureNeg :: (Eq a, Num a) => a -> a
ensureNeg e = if signum e == 1 then negate e else e

cwiidIsBtnEnabled flags btn =
  unCWiidBtnFlag flags .&. unCWiidBtnFlag btn == unCWiidBtnFlag btn
