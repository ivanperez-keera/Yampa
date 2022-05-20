-- |
-- Module      : testSpace
-- Description : Space tests.
-- Copyright   : Yale University, 2003
-- Authors     : Henrik Nilsson and Antony Courtney
module Main where

import FRP.Yampa

import TestsCommon
import TestsArr
import TestSpaceLoop
import TestSpacePSwitch
import TestSpaceRPSwitch
import TestSpaceRSwitch
import TestsAccum

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
