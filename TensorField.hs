module TensorField where

import Tensor
import Constraint

type TensorField = Position -> Tensor

makeBasisField :: Constraint -> TensorField
makeBasisField (Linear cp dir mag) p = error "not implemented"

makeTensorField :: [Constraint] -> TensorField
makeTensorField cs p = error "not implemented"
makeTensorField [] _ = Tensor 0 0
