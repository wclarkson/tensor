import qualified Tensor as T
import qualified Vector2 as V2
import Test.QuickCheck as QC

prop_eigenvectorsReal :: Float -> Float -> Bool
prop_eigenvectorsReal r t =
    let (ev1, ev2) = T.eigenvectors (T.fromRTheta r t)
    in V2.isReal ev1 && V2.isReal ev2

prop_eigenvectorsZeroTensorReal :: Float -> Bool
prop_eigenvectorsZeroTensorReal r =
  let (ev1, ev2) = T.eigenvectors (T.fromRTheta r 0)
  in V2.isReal ev1 && V2.isReal ev2

main :: IO ()
main = do
  QC.quickCheck prop_eigenvectorsReal
  QC.quickCheck prop_eigenvectorsZeroTensorReal
