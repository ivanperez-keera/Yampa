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
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.FRP.Yampa.Arrow        as Arrow
import qualified Test.FRP.Yampa.Basic        as Basic
import qualified Test.FRP.Yampa.Conditional  as Conditional
import qualified Test.FRP.Yampa.Delays       as Delays
import qualified Test.FRP.Yampa.Event        as Event
import qualified Test.FRP.Yampa.EventS       as EventS
import qualified Test.FRP.Yampa.Hybrid       as Hybrid
import qualified Test.FRP.Yampa.Integration  as Integration
import qualified Test.FRP.Yampa.InternalCore as InternalCore
import qualified Test.FRP.Yampa.Loop         as Loop
import qualified Test.FRP.Yampa.Random       as Random
import qualified Test.FRP.Yampa.Scan         as Scan
import qualified Test.FRP.Yampa.Simulation   as Simulation
import qualified Test.FRP.Yampa.Switches     as Switches
import qualified Test.FRP.Yampa.Task         as Task
import qualified Test.FRP.Yampa.Time         as Time

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Yampa QC properties"
  [ Arrow.tests
  , Basic.tests
  , Conditional.tests
  , Delays.tests
  , Event.tests
  , EventS.tests
  , Hybrid.tests
  , Integration.tests
  , InternalCore.tests
  , Loop.tests
  , Random.tests
  , Scan.tests
  , Simulation.tests
  , Switches.tests
  , Task.tests
  , Time.tests
  ]
