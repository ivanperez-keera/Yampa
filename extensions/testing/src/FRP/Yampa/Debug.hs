-- | Debug FRP networks by inspecting their behaviour inside.
module FRP.Yampa.Debug where

import Debug.Trace
import FRP.Yampa
import System.IO.Unsafe

-- | Signal Function that prints the value passing through using 'trace'.
traceSF :: Show a => SF a a
traceSF = traceSFWith show

-- | Signal Function that prints the value passing through using 'trace',
-- and a customizable 'show' function.
traceSFWith :: (a -> String) -> SF a a
traceSFWith f = arr (\x -> trace (f x) x)

-- | Execute an IO action using 'unsafePerformIO' at every step, and ignore the
-- result.
traceSFWithIO :: (a -> IO b) -> SF a a
traceSFWithIO f = arr (\x -> (unsafePerformIO (f x >> return x)))
