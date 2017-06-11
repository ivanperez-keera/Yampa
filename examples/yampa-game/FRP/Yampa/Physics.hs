{-# LANGUAGE Arrows #-}
module FRP.Yampa.Physics where

-- import Graphics.Rendering.OpenGL hiding (Radius)
import FRP.Yampa

type COR     = Float
type Acc2    = (Float,Float)
type Vel2    = (Float,Float)
type Pos2    = (Float,Float)
type Dir2    = (Float,Float)
type Color   = (Float, Float, Float)

data PhysicsContext = PhyC {
  gee :: Acc2,
  cor :: COR -- coefficient of restitution
}

defPhysics :: PhysicsContext
defPhysics = PhyC {
  gee = (0.0,-9.81),
  cor = 0.8
}

-- fallingBall pos0 vel0
-- ball with initial position and velocity.
freeFall :: Pos2 -> Vel2 -> SF () (Pos2, Vel2)
freeFall p0 v0 = proc () -> do
  v <- (v0 ^+^) ^<< integral -< gee defPhysics
  p <- (p0 ^+^) ^<< integral -< v
  returnA -< (p,v)
