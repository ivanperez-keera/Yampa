{- $Id: AFRPTests.hs,v 1.27 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTests                                            *
*       Purpose:        AFRP regression tests.				     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

-- TODO:
-- * Add test cases for AFRP. There should be at least one test case for each
--   "non-trivial" entity exported from AFRP.
--
-- * Make tests cases for after and repeatedly more robust.  Must not
--   fail due to small discrepancies in floating point implementation.
--
--   01-May-2002:  evsrc_t7 currently fails in hugs.
--
-- * Restructure test cases for papallel composition and switches to reflect
--   AFRP structure better. Separate test cases for the generic definitions?
-- There are some test cases for AFRPUtils. Not intended to be exhaustive.
--
-- VectorSpace has caused some ambiguity problems. See e.g. looplaws_t2,
-- switch_t1a.
--
-- 2005-11-26: A simple way of making many test cases more robust would
-- be to have a version of deltaEncode that adds a little extra time
-- to the very first delta time. That way sampling would always be slightly
-- "late".
--
-- But since we often compare time stamps, we'd also either have
-- to adjust the "~=" relation to tolerate "jitter" of that magnitute,
-- or we'd have to formulate many tests more carefully to allow a
-- certain "fuzziness".

module AFRPTests where

import FRP.Yampa

import AFRPTestsCommon
import AFRPTestsArr
import AFRPTestsComp
import AFRPTestsFirstSecond
import AFRPTestsLaws
import AFRPTestsLoop
import AFRPTestsLoopLaws
import AFRPTestsBasicSF
import AFRPTestsSscan
import AFRPTestsEvSrc
import AFRPTestsCOC
import AFRPTestsSwitch
import AFRPTestsKSwitch
import AFRPTestsRSwitch
import AFRPTestsPSwitch
import AFRPTestsRPSwitch
import AFRPTestsWFG
import AFRPTestsAccum
import AFRPTestsPre
import AFRPTestsDelay
import AFRPTestsDer
import AFRPTestsLoopPre
import AFRPTestsLoopIntegral
import AFRPTestsReact
import AFRPTestsEmbed
import AFRPTestsUtils
import AFRPTestsTask


------------------------------------------------------------------------------
-- Global test and error reporting
------------------------------------------------------------------------------

allGood = arr_tr
          && comp_tr
          && first_tr
          && second_tr
          && laws_tr
          && loop_tr
          && looplaws_tr
          && basicsf_tr
          && sscan_tr
          && evsrc_tr
 	  && coc_tr
 	  && switch_tr
 	  && kswitch_tr
 	  && rswitch_tr
 	  && pswitch_tr
 	  && rpswitch_tr
 	  && wfg_tr
	  && accum_tr
          && pre_tr
 	  && delay_tr
	  && der_tr
	  && loopPre_tr
	  && loopIntegral_tr
	  && react_tr
	  && embed_tr
	  && utils_tr
	  && task_tr


all_trs =
    [ ("arr",          arr_trs),
      ("comp",         comp_trs),
      ("first",        first_trs),
      ("second",       second_trs),
      ("laws",         laws_trs),
      ("loop",         loop_trs),
      ("looplaws",     looplaws_trs),
      ("basicsf",      basicsf_trs),
      ("sscan",	       sscan_trs),
      ("evsrc",        evsrc_trs),
      ("coc",          coc_trs),
      ("switch",       switch_trs),
      ("kswitch",      kswitch_trs),
      ("rswitch",      rswitch_trs),
      ("pswitch",      pswitch_trs),
      ("rpswitch",     rpswitch_trs),
      ("wfg",	       wfg_trs),
      ("accum",	       accum_trs),
      ("pre",	       pre_trs),
      ("delay",        delay_trs),
      ("der",          der_trs),
      ("loopPre",      loopPre_trs),
      ("loopIntegral", loopIntegral_trs),
      ("react",        react_trs),
      ("embed",        embed_trs),
      ("utils",        utils_trs),
      ("task",         task_trs)
    ]


failedTests =
    [ format n i | (n, trs) <- all_trs, (i, tr) <- zip [0..] trs, not tr ]
    where
	format n i = "Test " ++ n ++ "_t" ++ show i ++ " failed."


runRegTests :: IO Bool
runRegTests = do
    putStrLn ""
    putStrLn "Running the AFRP regression tests ..."
    if allGood
      then putStrLn "All tests succeeded!"
      else mapM_ putStrLn failedTests
    return allGood

runSpaceTests :: IO ()
runSpaceTests = do
    putStrLn ""
    putStrLn "Running the AFRP space tests ..."
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



