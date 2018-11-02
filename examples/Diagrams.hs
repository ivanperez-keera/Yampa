{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Example of connecting the diagrams drawing library with Yampa.
--
-- Based on:
-- https://archives.haskell.org/projects.haskell.org/diagrams/gallery/VectorField.html
--
-- Install diagrams with Cairo support, together with Yampa:
--
-- cabal sandbox init
-- cabal install Yampa diagrams -fcairo
--
-- Compile in a sandbox with:
--
-- cabal exec -- ghc --make examples/Diagrams.hs
--
-- And run with:
--
-- ./examples/Diagrams -w 400 -h 400 -o output.gif

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import FRP.Yampa                      hiding (norm, ( # ), (*^))

main = mainWith $ take 60 frames

frames :: [(Diagram B, Int)]
frames = zip ((embed sfVF $ deltaEncode 1 $ repeat ())) (repeat 1)

sfVF :: SF () (Diagram B)
sfVF = proc () -> do
  t <- time -< ()
  let diag = ( field t # translateY 0.05 # lc white
          <> ( square 3.5 # lw none # alignBL))
  returnA -< diag

field t = position $ zip points (arrows t)

locs   = [(x, y) | x <- [0.1, 0.3 .. 3.25], y <- [0.1, 0.3 .. 3.25]]

points = map p2 locs

vectorField t (x, y) = r2 (sin (t + y + 1), sin (t + x + 1))

arrows t = map (arrowAtPoint t) locs

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
