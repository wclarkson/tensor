module TensorField where

import qualified Tensor as T
import Tensor (Tensor)
import qualified Vector2 as V2
import Vector2 (Vector2 (Vector2))
import Constraint
import SVGWriter

-- many calculations are described in
-- Wonka: {http://peterwonka.net/Publications/pdfs/2008.SG.Chen.
--				InteractiveProceduralStreetModeling.pdf}

-- Constants
decayConstant :: Float
decayConstant = 0.10
cycleThreshold :: Float
cycleThreshold = 0.1

-- Drawing Specs

eigenvectorLine :: Vector2 -> Vector2 -> SVGElem
eigenvectorLine (Vector2 x0 y0) (Vector2 x1 y1) =
  Line x0 y0 x1 y1 "black" 0.05

constraintLine :: Vector2 -> Vector2 -> SVGElem
constraintLine (Vector2 x0 y0) (Vector2 x1 y1) =
  Line x0 y0 x1 y1 "red" 0.1

constraintCircle :: Vector2 -> SVGElem
constraintCircle (Vector2 x0 y0) = Circle x0 y0 0.5 "red" 0.1

-- Data Definition 

type TensorField = V2.Vector2 -> Tensor
type VectorField = V2.Vector2 -> V2.Vector2

-- combines the basis fields produced by the set of constraints into a
-- single tensor field
makeTensorField :: [Constraint] -> TensorField
makeTensorField [] _ = T.trivial
makeTensorField cs p = T.sum (map (basisFieldAtPoint p) cs)

-- as defined in Wonka for radial and linear constraints
basisFieldAtPoint :: Vector2 -> Constraint -> Tensor
basisFieldAtPoint pos constraint =
  let k = exp (-1 * decayConstant * V2.sqMag (V2.sub pos (posn constraint)))
      Vector2 xp yp = pos
      vector = case constraint of
                (Linear _ cdir cmag)  	 -> T.fromRTheta cmag cdir
                (Radial (Vector2 x0 y0)) -> T.fromXY (xp - x0) (yp - y0)
  in T.scalarTimes k vector

tensorfieldEigenvectors :: TensorField -> (VectorField, VectorField)
tensorfieldEigenvectors tf =
  let tfEv p = T.eigenvectors (tf p)
  in (fst . tfEv, snd . tfEv)

-- produces a list of n streamlines for a given vector and a vectorfield,
-- step and len are lengths such that n * step = len
traceStreamline :: VectorField -> Float -> Float -> Vector2 -> Float -> Float -> [Vector2]
traceStreamline vf w h p0 step len =
  let mkStep ps     0 = ps
      mkStep []     _ = []
      mkStep ((p,vlast):pvls) n =
        let v       = vf p
            v'      = if V2.dot v vlast > 0 then v
                                         else V2.scalarTimes (-1) v
            cycle   = (V2.mag (V2.sub p p0) < cycleThreshold) &&
                    (len - ((n+10) * step)) > cycleThreshold
            inBound = V2.inBounds p V2.zero (V2.Vector2 w h)
            p'      = V2.add p (V2.scalarTimes step (V2.unit v'))
        in if inBound then if cycle then mkStep ((p0,vlast):(p,vlast):pvls) 0
                                    else mkStep ((p',v'):(p,vlast):pvls) (n - 1)
                      else mkStep ((p',v'):(p,vlast):pvls) 0
  in map fst (mkStep [(p0, vf p0)] (len / step))

-- produces an SVG to draw a standard [res x res] tensor field and the 
-- given constraints
plotTensorField :: TensorField -> [Constraint] -> Float -> Float -> SVG
plotTensorField tf cs w h =
  let samplePts      = [ V2.Vector2 vx vy | vx<-[1..w], vy<-[1..h] ]
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
  in SVG w h (map mkVecLine plotVectors ++ map mkCons cs)


