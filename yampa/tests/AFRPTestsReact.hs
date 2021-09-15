{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsReact.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsReact					     *
*       Purpose:        Test cases for reactimation			     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsReact (react_tr, react_trs) where

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, writeIORef, readIORef)

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for reactimation
------------------------------------------------------------------------------

react_t0 :: [(Double, Double)]
react_t0 = unsafePerformIO $ do
    countr   <- newIORef undefined
    inputr   <- newIORef undefined
    outputsr <- newIORef []
    let init = do
	    writeIORef countr 1
	    let input0 = 0.0
            writeIORef inputr input0
	    return input0
        sense _ = do
	    count <- readIORef countr
	    if count >= 5 then do
		writeIORef countr 1
		input <- readIORef inputr
		let input' = input + 0.5
		writeIORef inputr input'
		return (0.1, Just input')
	     else do
		writeIORef countr (count + 1)
		return (0.1, Nothing)
	actuate _ output = do
	    outputs <- readIORef outputsr
	    writeIORef outputsr (output : outputs)
	    input <- readIORef inputr
	    return (input > 5.0)
    reactimate init sense actuate (arr dup >>> second integral)
    outputs <- readIORef outputsr
    return (take 25 (reverse outputs))
    

react_t0r :: [(Double, Double)]
react_t0r = [
    (0.0,0.00), (0.0,0.00), (0.0,0.00), (0.0,0.00), (0.0,0.00),
    (0.5,0.00), (0.5,0.05), (0.5,0.10), (0.5,0.15), (0.5,0.20),
    (1.0,0.25), (1.0,0.35), (1.0,0.45), (1.0,0.55), (1.0,0.65),
    (1.5,0.75), (1.5,0.90), (1.5,1.05), (1.5,1.20), (1.5,1.35),
    (2.0,1.50), (2.0,1.70), (2.0,1.90), (2.0,2.10), (2.0,2.30)]


react_trs = [ react_t0 ~= react_t0r ]


react_tr = and react_trs
