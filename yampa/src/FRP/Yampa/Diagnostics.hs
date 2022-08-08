-- |
-- Module      :  FRP.Yampa.Diagnostics
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  portable
--
-- Standardized error-reporting for Yampa
module FRP.Yampa.Diagnostics where

-- | Reports an error due to a violation of Yampa's preconditions/requirements.
usrErr :: String -> String -> String -> a
usrErr mn fn msg = error (mn ++ "." ++ fn ++ ": " ++ msg)

-- | Reports an error in Yampa's implementation.
intErr :: String -> String -> String -> a
intErr mn fn msg = error ("[internal error] " ++ mn ++ "." ++ fn ++ ": "
                           ++ msg)
