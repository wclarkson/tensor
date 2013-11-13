module TensorField where

import Tensor as T
import Vector2 as V2
import Constraint

-- Constants
decayConstant :: Float
decayConstant = 1

type TensorField = Vector2 -> Tensor

makeTensorField :: [Constraint] -> TensorField
makeTensorField cs p = T.sum (map (basisFieldAtPoint p) cs)
makeTensorField [] _ = T.trivial

basisFieldAtPoint :: Vector2 -> Constraint -> Tensor
basisFieldAtPoint pos (Linear cpos cdir cmag) =
  let c = exp (-1 * decayConstant * V2.sqMag (V2.sub pos cpos))
      basisField = T.fromRTheta cmag cdir
  in T.scalarTimes c basisField

