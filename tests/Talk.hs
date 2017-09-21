import TemporalLogic
import FRP.Yampa
import Test.QuickCheck
import SampleStreams
import SampleStreamsQC

greaterThan :: SF (Int, Int) Bool
greaterThan = arr $ \(x,y) -> x > y

alwaysGreater :: TPred (Int, Int)
alwaysGreater = Always $ Prop greaterThan

-- > evalT alwaysGreater ((5,1), [(0.001, (6, 1)), (0.001, (9, 2))])
-- True
eval1 = evalT alwaysGreater ((5,1), [(0.001, (6, 1)), (0.001, (9, 2))])

-- > evalT alwaysGreater ((1,5), [(0.001, (6, 1)), (0.001, (9, 2))])
-- False
eval2 = evalT alwaysGreater ((1,5), [(0.001, (6, 1)), (0.001, (9, 2))])

alwaysGreaterProperty :: Property
alwaysGreaterProperty = forAll arbitrary (evalT alwaysGreater)

evalQ1 = quickCheck alwaysGreaterProperty
