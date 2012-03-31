{- $Id: testAFRPMain.hs,v 1.9 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         testAFRPMain                                         *
*       Purpose:        Main driver routine for running tests.               *
*	Authors:	Henrik Nilsson and Antony Courtney		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}
module Main where

import AFRPTests

import System.IO
import System.Environment (getArgs, getProgName)

-- main = runTests
-- main = runSpaceTests

data TestFlags = TestFlags { tReg :: Bool -- run regression tests
			   , tSpace :: Bool -- run space tests
			   , tHelp :: Bool -- print usage and exit
			     }

defFlags = TestFlags { tReg = False, tSpace = False, tHelp = False}
allFlags = TestFlags { tReg = True, tSpace = True, tHelp = False}

parseArgs :: TestFlags -> [String] -> Either TestFlags String
parseArgs flags [] = Left flags
parseArgs flags (arg:args) =
  case arg of
    "-r" -> parseArgs (flags {tReg = True}) args
    "-s" -> parseArgs (flags {tSpace = True}) args
    "-h" -> parseArgs (flags {tHelp = True}) args
    _ -> Right ("invalid argument: " ++ arg)

usage :: String -> Maybe String -> IO ()
usage pname mbEmsg = do
  case mbEmsg of
    (Just emsg) -> hPutStrLn stderr (pname ++ ": " ++ emsg)
    _ -> return ()
  hPutStrLn stderr ("usage: " ++ pname ++ " [-r] [-s] [-h]")
  hPutStrLn stderr "\t-s run space tests"
  hPutStrLn stderr "\t-r run regression tests"
  hPutStrLn stderr "\t-h print this help message"
  hPutStrLn stderr "(no arguments runs all tests.)"

main :: IO ()
main = do
  pname <- getProgName
  args <- getArgs
  let eFlags = if (length args) < 1 
                 then (Left allFlags)
                 else parseArgs defFlags args
  case eFlags of
    (Left tFlags) ->  
      if (tHelp tFlags)
        then usage pname Nothing
        else do
          if (tReg tFlags)
            then runRegTests
            else return ()
          if (tSpace tFlags)
            then runSpaceTests
            else return ()
    (Right emsg) -> usage pname (Just emsg)


