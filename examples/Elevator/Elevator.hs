{-# LANGUAGE Arrows #-}

{-
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         Elevator					     *
*       Purpose:        Elevator simulation based on the Fran version	     *
*			from Simon Thompson's paper "A functional reactive   *
*			animation of a lift using Fran".		     *
*	Authors:	Henrik Nilsson					     *
*                                                                            *
*             Copyright (c) The University of Nottingham, 2004		     *
*                                                                            *
******************************************************************************
-}

module Elevator where

import FRP.Yampa
import FRP.Yampa.Utilities -- ((^<<), dHold)

------------------------------------------------------------------------------
-- Auxiliary definitions
------------------------------------------------------------------------------

type Position = Double	-- [m]
type Distance = Double	-- [m]
type Velocity = Double	-- [m/s]


------------------------------------------------------------------------------
-- Elevator simulator
------------------------------------------------------------------------------

lower, upper :: Position
lower = 0
upper = 5

upRate, downRate :: Velocity
upRate = 1
downRate = 1.1


elevator :: SF (Event (), Event ()) Position
elevator = proc (lbp,rbp) -> do
    rec
        -- This delayed hold can be thought of as modelling acceleration.
        -- It is not "physical" to expect a desire to travel at a certain
        -- velocity to be immediately reflected in the actual velocity.
        -- (The reason we get into trouble here is that the stop/go events
        -- depends instantaneously on "stopped" which in turn depends
        -- instantaneously on "v".)
        v <- dHold 0 -< stop    `tag` 0
                        `lMerge`
                        goUp    `tag` upRate
                        `lMerge`
                        goDown  `tag` (-downRate)
        
        y <- (lower +) ^<< integral -< v    
        
        let atBottom = y <= lower
            atTop    = y >= upper
            stopped  = v == 0		-- Somewhat dubious ...
        
            waitingBottom = atBottom && stopped
            waitingTop    = atTop    && stopped
        
        arriveBottom <- edge -< atBottom
        arriveTop    <- edge -< atTop
        
        let setUp   = lbp `tag` True
            setDown = rbp `tag` True
        
        -- This does not work. The reset events would be generated as soon
        -- as the corresponding go event was generated, but the latter
        -- depend instantaneusly on the reset signals.
--          resetUp   = goUp `tag` False
--          resetDown = goDown `tag` False

	-- One approach would be to wait for "physical confiramtion"
	-- that the elevator actually is moving in the desired direction:
--	resetUp   <- (`tag` True)  ^<< edge -< v > 0
--      resetDown <- (`tag` False) ^<< edge -< v < 0

	-- Another approach is to simply delay the reset events to avoid
        -- suppressing the very event that generates the reset event.
	resetUp   <- iPre noEvent -< goUp `tag` False
        resetDown <- iPre noEvent -< goDown `tag` False

        -- Of course, a third approach would be to just use dHold below.
        -- But that does not seem to be the right solution to me.
        upPending   <- hold False -< setUp   `lMerge` resetUp
        downPending <- hold False -< setDown `lMerge` resetDown
        
        let pending = upPending || downPending
            eitherButton = lbp `lMerge` rbp
        
            goDown  = arriveTop `gate` pending
                      `lMerge`
                      eitherButton `gate` waitingTop
            goUp    = arriveBottom `gate` pending
                      `lMerge`
                      eitherButton `gate` waitingBottom
            stop    = (arriveTop `lMerge` arriveBottom) `gate` not pending
        
    returnA -< y
