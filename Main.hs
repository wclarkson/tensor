import Tensor
import TensorField
import Constraint
import Vector2 (Vector2 (Vector2))
import SVGWriter
import qualified NearestNeighbor as NN
import System.Random

import Debug.Trace

cs :: [Constraint]
cs = [ --Radial (Vector2 10 10),
       Linear (Vector2 2 2)  0.01 3,
       Linear (Vector2 6 14) 0.6  3,
       Linear (Vector2 8 8)  1.2  3 ]

cs' :: [Constraint]
cs' = [ Linear (Vector2 10 15) 0.0 3,
        Linear (Vector2 15 10) 1.3 3]

type Seed = Vector2

fieldWidth :: Float
fieldWidth = 20
fieldHeight :: Float
fieldHeight = 20

tf :: TensorField
tf = makeTensorField cs

traceMajorLine :: (Num a) => Vector2 -> NN.Storage a -> (SVGElem, NN.Storage a)
traceMajorLine v = traceLine "green" fst v

traceMinorLine :: (Num a) => Vector2 -> NN.Storage a -> (SVGElem, NN.Storage a)
traceMinorLine v = traceLine "blue" snd v

getStreamline :: (Num a) => ((VectorField, VectorField) -> VectorField) -> 
  NN.Storage a -> Vector2 -> [Vector2]
getStreamline which nn v = 
  let mEvs     = which (tensorfieldEigenvectors tf)
  in  traceStreamline mEvs nn fieldWidth fieldHeight v 0.01 75

traceLine :: (Num a) => String -> ((VectorField, VectorField) -> VectorField) 
  -> Vector2 -> NN.Storage a -> (SVGElem, NN.Storage a)
traceLine color which v nn =
  let tracePts = getStreamline which nn v
      nn'      = foldr (flip NN.insert) nn (map (\p -> (p, 0)) tracePts)
      vecToPair (Vector2 vx vy) = (vx, vy)
  in (Polyline (map vecToPair tracePts) color 0.1, nn')

randomSeeds :: [Vector2]
randomSeeds =
  let (g,g') = split (mkStdGen 9)
      xs     = randomRs (0.0,fieldWidth) g
      ys     = randomRs (0.0,fieldHeight) g'
  in take 10 (map (uncurry Vector2) (zip xs ys))

traceLines :: [SVGElem]
traceLines = 
  let gatherMajor (elems, nn) v = (elem:elems, nn')
        where (elem, nn') = traceMajorLine v nn
      gatherMinor (elems, nn) v = (elem:elems, nn')
        where (elem, nn') = traceMinorLine v nn
      nn0          = NN.new fieldWidth fieldHeight 5
      (majElems, _) = foldl gatherMajor ([], nn0) randomSeeds
      (minElems, _) = foldl gatherMinor ([], nn0) randomSeeds
  in majElems ++ minElems

main :: IO ()
main = putStrLn (writeSVG
                  (appendElements
                  (plotTensorField tf cs fieldWidth fieldHeight)
                  traceLines))
