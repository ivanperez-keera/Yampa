module FRP.Yampa.Debug where

import Debug.Trace
import FRP.Yampa
import System.IO.Unsafe

-- ** Debugging

traceSF :: Show a
        => SF a a
traceSF = traceSFWith show

traceSFWith :: (a -> String)
            -> SF a a
traceSFWith f = arr (\x -> trace (f x) x)

traceSFWithIO :: (a -> IO b)
              -> SF a a
traceSFWithIO f = arr (\x -> (unsafePerformIO (f x >> return x)))
