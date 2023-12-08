{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- |
-- Copyright   :  (c) Ivan Perez, 2018-2022
-- License     :  BSD-style (see the LICENSE file in the distribution)
-- Maintainer  :  ivan.perez@keera.co.uk
--
-- Example of connecting the diagrams drawing library with Yampa.
--
-- Based on:
-- https://archives.haskell.org/projects.haskell.org/diagrams/gallery/VectorField.html
--
-- Install diagrams with Cairo support, together with Yampa:
--
-- cabal v1-sandbox init
-- cabal v1-install Yampa diagrams diagrams-cairo
--
-- Compile in a sandbox with:
--
-- cabal v1-exec -- ghc --make examples/Diagrams.hs
--
-- And run with:
--
-- ./examples/Diagrams -w 400 -h 400 -o output.gif

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude               hiding (Time)
import FRP.Yampa                      hiding (norm, ( # ), (*^))

main :: IO ()
main = mainWith $ take 60 frames

-- | Frames of the animation.
frames :: [(Diagram B, Int)]
frames = zip ((embed sfVF $ deltaEncode 1 $ repeat ())) (repeat 1)

-- | Signal producing the diagram at a point in time.
sfVF :: SF () (Diagram B)
sfVF = proc () -> do
  t <- time -< ()
  let diag = ( field t # translateY 0.05 # lc white
          <> ( square 3.5 # lw none # alignBL))
  returnA -< diag

-- | Field of arrows as it changes over time.
field :: Time -> Diagram B
field t = position $ zip points (arrows t)

-- | Arrow points as they change over time.
points :: [Point V2 Double]
points = map p2 locs

-- | Arrow locations as they change over time.
locs   :: [(Double, Double)]
locs   = [(x, y) | x <- [0.1, 0.3 .. 3.25], y <- [0.1, 0.3 .. 3.25]]

-- | Arrows as they change over time.
arrows :: Time -> [Diagram B]
arrows t = map (arrowAtPoint t) locs

-- | Diagram of a star at a given point in time and space.
arrowAtPoint :: Time -> (Double, Double) -> Diagram B
arrowAtPoint t (x, y) = arrowAt' opts (p2 (x, y)) (sL *^ vf) # alignTL
  where
    vf   = vectorField t (x, y)
    m    = norm $ vectorField t (x, y)

    -- Head size is a function of the length of the vector
    -- as are tail size and shaft length.

    hs   = 0.02 * m
    sW   = 0.004 * m
    sL   = 0.05 + 0.1 * m
    opts = (with & arrowHead  .~ spike
                 & headLength .~ normalized hs
                 & shaftStyle %~ lwN sW)

-- | Direction vector depending on the time and the position in space.
vectorField :: Time -> (Double, Double) -> V2 Double
vectorField t (x, y) = r2 (sin (t + y + 1), sin (t + x + 1))
