module TensorField where

import Tensor as T
import Vector2 as V2
import Constraint
import SVGWriter

-- Constants
decayConstant :: Float
decayConstant = 1

type TensorField = Vector2 -> Tensor

makeTensorField :: [Constraint] -> TensorField
makeTensorField [] _ = T.trivial
makeTensorField cs p = T.sum (map (basisFieldAtPoint p) cs)

basisFieldAtPoint :: Vector2 -> Constraint -> Tensor
basisFieldAtPoint pos (Linear cpos cdir cmag) =
  let c          = exp (-1 * decayConstant * V2.sqMag (V2.sub pos cpos))
      basisField = T.fromRTheta cmag cdir
  in T.scalarTimes c basisField
basisFieldAtPoint _ _ = error "not implemented"

plotTensorField :: TensorField -> [Constraint] -> Float -> SVG
plotTensorField tf cs res =
  let samplePts    = [ Vector2 vx vy | vx<-[1..res], vy<-[1..res] ]
      tensorVals   = map tf samplePts
      tensorEvs    = map eigenvectors tensorVals
      majorEvs     = map (V2.scalarTimes 0.3 . unit . fst) tensorEvs
      plotVectors  = zip samplePts majorEvs
      mkVecLine (Vector2 px py, Vector2 evx evy) =
        Line px py (px+evx) (py+evy) "black" 0.01
      mkConsLine (Linear (Vector2 cpx cpy) cdir cmag) = 
        let cx = cmag * cos cdir
            cy = cmag * sin cdir
        in Line cpx cpy (cpx+cx) (cpy+cy) "red" 0.1
      mkConsLine _ = error "not implemented"
  in SVG res res ((map mkVecLine plotVectors) ++ (map mkConsLine cs))


