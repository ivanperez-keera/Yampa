-- |
-- Module      : testSpace
-- Description : Space tests.
-- Copyright   : Yale University, 2003
-- Authors     : Henrik Nilsson and Antony Courtney
module Main where

import FRP.Yampa
import Data.List (findIndex)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, writeIORef, readIORef)

import TestsCommon (REq(..))

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Running the Yampa space tests ..."
    putStrLn "Testing the space behaviour. This may take a LONG time."
    putStrLn "Observe the process size using some tool like top."
    putStrLn "The process should not grow significantly."
    putStrLn "Emitted success/failure indications signify termination"
    putStrLn "and whether or not the right result was obtained. They do"
    putStrLn "not necessarily indicate that the space behaviour is correct"
    putStrLn "(i.e., absence of leaks)."
    putStrLn ""
    rst "arr" 0 arr_st0 arr_st0r
    rst "arr" 1 arr_st1 arr_st1r
    rst "loop" 0 loop_st0 loop_st0r
    rst "loop" 1 loop_st1 loop_st1r
    rst "rswitch" 0 rswitch_st0 rswitch_st0r
    rst "pswitch" 0 pswitch_st0 pswitch_st0r
    rst "pswitch" 1 pswitch_st1 pswitch_st1r
    rst "rpswitch" 0 rpswitch_st0 rpswitch_st0r
    rst "accum" 0 accum_st0 accum_st0r
    rst "accum" 1 accum_st1 accum_st1r
    where
        rst n i st str = do
            putStrLn ("Running " ++ n ++ "_st" ++ show i ++ " ...")
            if st ~= str then
                putStrLn "Success!"
             else
                -- We probably won't get here in case of a (space) failure ...
                putStrLn "Failure!"

-- AC: here because I had trouble running ghci:
-- fixTest :: IO ()
-- fixTest =
--   let vs = loop_t17
--   in putStrLn ("loop_t17 output: " ++ show vs)

-- * Test cases for arr

arr_st0 = testSFSpaceLeak 2000000 (arr (+1))
arr_st0r = 1000000.5

arr_st1 = testSFSpaceLeak 2000000 identity
arr_st1r = 999999.5

-- * Test cases for loop

loop_acc :: SF (Double, Double) (Double, Double)
loop_acc = arr (\(x, y)->(x+y, x+y))

loop_st0 = testSFSpaceLeak 2000000
                           (loop (second (iPre 0) >>> loop_acc))
loop_st0r = 9.999995e11

-- A simple loop test taken from MiniYampa:
-- This results in pulling on the fed-back output during evaluation, because
-- switch is strict in its input sample:
loop_st1 :: Double
loop_st1 = testSFSpaceLeak 2000000
             (loop $ second $ (switch identity (const (arr fst))) >>> arr (\x -> (x + x + x + x + x + x + x,noEvent)) >>> (iPre (25, noEvent)))
loop_st1r = 999999.5

-- * Test cases for rSwitch and drSwitch

rswitch_sawTooth :: SF a Double
rswitch_sawTooth =
    loop (second (arr (>=5.0)
                  >>> edge
                  >>> arr (`tag` ramp))
          >>> drSwitch ramp
          >>> arr dup)
  where
    ramp :: SF a Double
    ramp = constant 1.0 >>> integral

rswitch_st0 = testSFSpaceLeak 2000000 rswitch_sawTooth
rswitch_st0r = 4.75

-- * Test cases for pSwitchB and dpSwitchB

-- Starts three "ramps" with different phase. As soon as one exceeds a
-- threshold, it's restarted, while the others are left alone. The
-- observaton of the output is done via the loop (rather than the directly
-- from the outputs of the signal functions in the collection), thus the
-- use of a delayed switch is essential.
pswitch_ramp :: Double -> SF a Double
pswitch_ramp phase = constant 2.0 >>> integral >>> arr (+phase)

-- We assume that only one signal function will reach the limit at a time.
pswitch_limit :: Double -> SF ((a, [Double]), b) (Event Int)
pswitch_limit x = arr (snd . fst) >>> arr (findIndex (>=x)) >>> edgeJust

pswitch_t4rec :: [SF (a, [Double]) Double]
                 -> Int
                 -> SF (a, [Double]) [Double]
pswitch_t4rec sfs n =
  dpSwitchB (take n sfs ++ [pswitch_ramp 0.0] ++ drop (n+1) sfs)
            (pswitch_limit 2.99)
            pswitch_t4rec

-- Variation of the test above, with direct observation (not via loop) and
-- immediate switch.
--
-- We assume that only one signal function will reach the limit at a time.
pswitch_limit2 :: Double -> SF (a, [Double]) (Event Int)
pswitch_limit2 x = arr snd >>> arr (findIndex (>=x)) >>> edgeJust

pswitch_t5rec :: [SF (a, [Double]) Double]
                 -> Int
                 -> SF (a, [Double]) [Double]
pswitch_t5rec sfs n =
  pSwitchB (take n sfs ++ [pswitch_ramp 0.0] ++ drop (n+1) sfs)
           (pswitch_limit2 2.99)
           pswitch_t5rec

pswitch_st0 = testSFSpaceLeak 1000000 (loop sf)
  where
    sf :: SF (a, [Double]) ([Double],[Double])
    sf = dpSwitchB [pswitch_ramp 0.0, pswitch_ramp 1.0, pswitch_ramp 2.0]
                   (pswitch_limit 2.99)
                   pswitch_t4rec
         >>> arr dup

pswitch_st0r = [1.5,2.5,0.5]

pswitch_st1 = testSFSpaceLeak 1000000 (loop sf)
  where
    sf :: SF (a, [Double]) (([Double], Double), [Double])
    sf = ((pSwitchB [pswitch_ramp 0.0, pswitch_ramp 1.0, pswitch_ramp 2.0]
                    (pswitch_limit2 2.99)
                    pswitch_t5rec)
          &&& (arr snd >>> arr sum))
         >>> arr (\(xs, y) -> ((xs, y), xs))

pswitch_st1r = ([1.5,2.5,0.5],4.5)

-- * Test cases for rpSwitchB and drpSwitchB

-- Starts three "ramps" with different phase. As soon as one exceeds a
-- threshold, it's restarted, while the others are left alone. The observaton
-- of the output is done via a loop, thus the  use of a delayed switch is
-- essential.

rpswitch_ramp :: Double -> SF a Double
rpswitch_ramp phase = constant 2.0 >>> integral >>> arr (+phase)

-- We assume that only one signal function will reach the limit at a time.
rpswitch_limit :: Double -> SF [Double] (Event ([SF a Double]->[SF a Double]))
rpswitch_limit x = arr (findIndex (>=x)) >>> edgeJust >>> arr (fmap restart)
  where
    restart n = \sfs -> take n sfs ++ [rpswitch_ramp 0.0] ++ drop (n+1) sfs

rpswitch_st0 = testSFSpaceLeak 1000000 (loop sf)
  where
    sf :: SF (a, [Double]) ([Double],[Double])
    sf = (second (rpswitch_limit 2.99)
          >>> drpSwitchB [ rpswitch_ramp 0.0
                         , rpswitch_ramp 1.0
                         , rpswitch_ramp 2.0
                         ]
         ) >>> arr dup

rpswitch_st0r = [1.5,2.5,0.5]

-- * Test cases for accumulators

accum_st0 :: Double
accum_st0 = testSFSpaceLeak 1000000
                            (repeatedly 1.0 1.0
                             >>> accumBy (+) 0.0
                             >>> hold (-99.99))

accum_st0r = 249999.0

accum_st1 :: Double
accum_st1 = testSFSpaceLeak 1000000
                            (arr dup
                             >>> first (repeatedly 1.0 1.0)
                             >>> arr (\(e,a) -> tag e a)
                             >>> accumFilter accumFun 0.0
                             >>> hold (-99.99))
  where
    accumFun c a | even (floor a) = (c+a, Just (c+a))
                 | otherwise      = (c, Nothing)

accum_st1r = 6.249975e10

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
