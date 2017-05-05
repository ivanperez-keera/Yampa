{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsCommon.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsCommon                                      *
*       Purpose:        Common definitions for the regression test modules.  *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsCommon where

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, writeIORef, readIORef)

import FRP.Yampa

------------------------------------------------------------------------------
-- Rough equality with instances
------------------------------------------------------------------------------

-- Rough equality. Only intended to be good enough for test cases in this
-- module.

class REq a where
    (~=) :: a -> a -> Bool

epsilon :: Fractional a => a
epsilon = 0.0001

instance REq Float where
    x ~= y = abs (x - y) < epsilon	-- A relative measure should be used.

instance REq Double where
    x ~= y = abs (x - y) < epsilon	-- A relative measure should be used.

instance REq Int where
    (~=) = (==) 

instance REq Integer where
    (~=) = (==) 

instance REq Bool where
    (~=) = (==) 

instance REq Char where
    (~=) = (==) 

instance REq () where
    () ~= () = True

instance (REq a, REq b) => REq (a,b) where
    (x1,x2) ~= (y1,y2) = x1 ~= y1 && x2 ~= y2

instance (REq a, REq b, REq c) => REq (a,b,c) where
    (x1,x2,x3) ~= (y1,y2,y3) = x1 ~= y1 && x2 ~= y2 && x3 ~= y3

instance (REq a, REq b, REq c, REq d) => REq (a,b,c,d) where
    (x1,x2,x3,x4) ~= (y1,y2,y3,y4) = x1 ~= y1
				     && x2 ~= y2
				     && x3 ~= y3
				     && x4 ~= y4

instance (REq a, REq b, REq c, REq d, REq e) => REq (a,b,c,d,e) where
    (x1,x2,x3,x4,x5) ~= (y1,y2,y3,y4,y5) = x1 ~= y1
				           && x2 ~= y2
				           && x3 ~= y3
				           && x4 ~= y4
				           && x5 ~= y5

instance REq a => REq (Maybe a) where
    Nothing ~= Nothing   = True
    (Just x) ~= (Just y) = x ~= y
    _        ~= _        = False

instance REq a => REq (Event a) where
    NoEvent   ~= NoEvent   = True
    (Event x) ~= (Event y) = x ~= y
    _         ~= _         = False

instance (REq a, REq b) => REq (Either a b) where
    (Left x)  ~= (Left y)  = x ~= y
    (Right x) ~= (Right y) = x ~= y
    _         ~= _         = False

instance REq a => REq [a] where
    [] ~= []         = True
    (x:xs) ~= (y:ys) = x ~= y && xs ~= ys
    _      ~= _      = False


------------------------------------------------------------------------------
-- Testing utilities
------------------------------------------------------------------------------

testSF1 :: SF Double a -> [a]
testSF1 sf = take 25 (embed sf (deltaEncodeBy (~=) 0.25 [0.0..]))


testSF2 :: SF Double a -> [a]
testSF2 sf = take 25 (embed sf (deltaEncodeBy (~=) 0.25 input))
    where
	-- The initial 0.0 is just for result compatibility with an older
	-- version.
	input = 0.0 : [ fromIntegral (b `div` freq) | b <- [1..] :: [Int] ]
	freq = 5


------------------------------------------------------------------------------
-- Test harness for space behaviour 
------------------------------------------------------------------------------

{-
-- Test for space leaks.
-- Carefully defined in an attempt to defeat fully lazy lambda lifting.
-- Seems to work, but may be unsafe if the compiler decides to optimize
-- aggressively.
testSFSpaceLeak :: Int -> SF Double a -> a
testSFSpaceLeak n sf = embed sf (deltaEncodeBy (~=) 0.25 [(seq n 0.0)..]) !! n
-}


-- Using embed/deltaEncode seems to be a bad idea since fully lazy
-- lambda lifting often results in lifting a big input list to the top
-- level in the form of a CAF. Using reactimate and avoiding constructing
-- input/output lists should be more robust.

testSFSpaceLeak :: Int -> SF Double a -> a
testSFSpaceLeak n sf = unsafePerformIO $ do
    countr  <- newIORef 0
    inputr  <- newIORef undefined
    outputr <- newIORef undefined
    let init = do
	    let input0 = 0.0
            writeIORef inputr input0
	    count <- readIORef countr
	    writeIORef countr (count + 1)
	    return input0
        sense _ = do
	    input <- readIORef inputr
	    let input' = input + 0.5
	    writeIORef inputr input'
	    count <- readIORef countr
	    writeIORef countr (count + 1)
	    return (0.25, Just input')
	actuate _ output = do
	    writeIORef outputr output
	    _input <- readIORef inputr
	    count  <- readIORef countr
	    return (count >= n)
    reactimate init sense actuate sf

    -- return output
    readIORef outputr

------------------------------------------------------------------------------
-- Some utilities used for testing laws
------------------------------------------------------------------------------

assoc :: ((a,b),c) -> (a,(b,c))
assoc ((a,b),c) = (a,(b,c))

assocInv :: (a,(b,c)) -> ((a,b),c)
assocInv (a,(b,c)) = ((a,b),c)
