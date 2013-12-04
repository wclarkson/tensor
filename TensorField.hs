module TensorField where

import qualified Tensor as T
import Tensor (Tensor)
import qualified Vector2 as V2
import Vector2 (Vector2 (Vector2))
import Constraint
import SVGWriter
import NearestNeighbor (Storage, Point (Point))
import qualified NearestNeighbor as NN

import Data.Maybe
import Debug.Trace

-- many calculations are described in
-- Wonka: {http://peterwonka.net/Publications/pdfs/2008.SG.Chen.
--				InteractiveProceduralStreetModeling.pdf}

-- Constants
decayConstant :: Float
decayConstant = 0.10
cycleThreshold :: Float
cycleThreshold = 0.1
dSep :: Float
dSep = 0.15

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

-- produces a streamline for a given vector and a vectorfield,
-- step and len are lengths such that n * step = len
traceStreamline ::
  (Num a) => VectorField -> Storage a -> Float -> Float -> Vector2 -> Float -> Float -> [Vector2]
traceStreamline vf nn0 w h p0 step len =
  let mkStep ps     _     0 = ps
      mkStep []     _     _ = []
      mkStep (p:ps) vlast n =
        let v       = vf p
            v'      = if V2.dot v vlast > 0 then v
                                         else V2.scalarTimes (-1) v
            cycle   = (V2.mag (V2.sub p p0) < cycleThreshold) &&
                    (len - ((n+10) * step)) > cycleThreshold
            inBound = V2.inBounds p V2.zero (V2.Vector2 w h)
            point (V2.Vector2 x y) = (Point x y) -- this function is the best
            vec2  (Point x y) = (V2.Vector2 x y)
            lookup  = NN.lookup nn0 (point v)
            iSect   = case lookup of
                        (Just (nearest, _)) -> 
                          V2.mag (V2.sub (vec2 nearest) p) < dSep
                        Nothing             -> False
            p'      = V2.add p (V2.scalarTimes step (V2.unit v'))
            output | not inBound = mkStep (p':p:ps) v' 0
                   | cycle       = mkStep (p0:p:ps) vlast 0
                   | iSect       = mkStep ((vec2 $ fst $ fromJust lookup):p:ps) v' 0
                   | otherwise   = mkStep (p':p:ps) v' (n - 1)
          in output
      traceLine dir = mkStep [p0] (V2.scalarTimes dir (vf p0)) (len / step)
  in traceLine 1 ++ reverse (traceLine (-1))

-- I wasn't sure what 'a' to throw in the nearest neighbor construct, so
-- I put 0 and had it inherit from Num
addStreamlineToNN :: (Num a) => [Vector2] -> Storage a -> Storage a
addStreamlineToNN vectors stor = foldr ins stor vectors
  where ins (Vector2 x y) st = NN.insert st (Point x y, 0)

--newNNFromStreamlines ::
--  (Num a) => [[Vector2]] -> Float -> Float -> Int -> Storage a
--newNNFromStreamlines v2list w h nbux =
--    foldr addStreamlineToNN (NN.new w h nbux) v2list

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


