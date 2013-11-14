import Tensor
import qualified Vector2 as V2

-- for quickCheck
prop_eigenvectorsNeverNaN :: Float -> Float -> Float -> Bool
prop_eigenvectorsNeverNaN x y z =
    let (ev1, ev2) = eigenvectors (Tensor x y z)
        pass = not . V2.isNaN
    in pass ev1 && pass ev2
