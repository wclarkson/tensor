module TensorField where

import qualified Tensor as T
import Tensor (Tensor)
import qualified Vector2 as V2
import Vector2 (Vector2 (Vector2))
import Constraint
import SVGWriter

-- Constants
decayConstant :: Float
decayConstant = 0.10

eigenvectorLine :: Vector2 -> Vector2 -> SVGElem
eigenvectorLine (Vector2 x0 y0) (Vector2 x1 y1) =
  Line x0 y0 x1 y1 "black" 0.05

constraintLine :: Vector2 -> Vector2 -> SVGElem
constraintLine (Vector2 x0 y0) (Vector2 x1 y1) =
  Line x0 y0 x1 y1 "red" 0.1

constraintCircle :: Vector2 -> SVGElem
constraintCircle (Vector2 x0 y0) = Circle x0 y0 0.5 "red" 0.1

type TensorField = V2.Vector2 -> Tensor

type VectorField = V2.Vector2 -> V2.Vector2

makeTensorField :: [Constraint] -> TensorField
makeTensorField [] _ = T.trivial
makeTensorField cs p = T.sum (map (basisFieldAtPoint p) cs)

basisFieldAtPoint :: Vector2 -> Constraint -> Tensor
basisFieldAtPoint pos constraint =
  let cpos = case constraint of (Linear cp _ _) -> cp
                                (Radial cp)     -> cp
      k = exp (-1 * decayConstant * V2.sqMag (V2.sub pos cpos))
      Vector2 xp yp = pos
      vector = case constraint of
                (Linear _ cdir cmag)  -> T.fromRTheta cmag cdir
                (Radial (Vector2 x0 y0)) -> T.fromXY (xp - x0) (yp - y0)
  in T.scalarTimes k vector

tensorfieldEigenvectors :: TensorField -> (VectorField, VectorField)
tensorfieldEigenvectors tf =
  let tfEv p = T.eigenvectors (tf p)
  in (fst . tfEv, snd . tfEv)

traceStreamline :: VectorField -> Vector2 -> Float -> Int -> [Vector2]
traceStreamline vf p0 step iter =
  let mkStep ps     0 = ps
      mkStep []     _ = []
      mkStep ((p,vlast):pvls) n =
        let v   = vf p
            v'  = if V2.dot v vlast > 0 then v
                                         else V2.scalarTimes (-1) v
            p'  = V2.add p (V2.scalarTimes step (V2.unit v'))
        in  mkStep ((p',v'):(p,vlast):pvls) (n - 1)
  in map fst (mkStep [(p0, vf p0)] iter)

plotTensorField :: TensorField -> [Constraint] -> Float -> SVG
plotTensorField tf cs res =
  let samplePts      = [ V2.Vector2 vx vy | vx<-[1..res], vy<-[1..res] ]
      majorEvs       = map (fst (tensorfieldEigenvectors tf)) samplePts
      scaledMajorEvs = map (V2.scalarTimes 0.5 . V2.unit) majorEvs
      plotVectors  = zip samplePts scaledMajorEvs
      mkVecLine (p, ev) =
        eigenvectorLine p (V2.add p ev)
      mkCons (Linear cp cdir cmag) =
        let cx = cmag * cos cdir
            cy = cmag * sin cdir
        in constraintLine cp (V2.add cp (V2.Vector2 cx cy))
      mkCons (Radial cp) = constraintCircle cp
  in SVG res res (map mkVecLine plotVectors ++ map mkCons cs)


