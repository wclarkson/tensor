import Tensor
import TensorField
import Constraint
import Vector2 (Vector2 (Vector2))
import SVGWriter
import NearestNeighbor (Storage)
import qualified NearestNeighbor as NN

import Debug.Trace

cs :: [Constraint]
cs = [ Radial (Vector2 8 8), Linear (Vector2 2 2) 0.01 3,
       Linear (Vector2 6 14) 0.6 3]

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

traceLine :: (Num a) => Seed -> Storage a -> (SVGElem, Storage a)
traceLine seed nn =
  let mEvs     = fst (tensorfieldEigenvectors tf)
      tracePts = traceStreamline mEvs nn fieldWidth fieldHeight seed 0.01 75
      nn'      = addStreamlineToNN tracePts nn
      vecToPair (Vector2 vx vy) = (vx, vy)
  in (Polyline (map vecToPair tracePts) "green" 0.1, nn')

--traceLines :: [SVGElem]
--traceLines = [traceLine (Vector2 15 15)]
  --map traceLine [ Vector2 5 ty | ty<-[10..15] ]

traceLines :: [SVGElem]
traceLines = traceMultipleLines [(Vector2 15 15), (Vector2 3 3)] (NN.new fieldWidth fieldHeight 5)

-- v2 args are seeds
traceMultipleLines :: (Num a) => [Seed] -> Storage a -> [SVGElem]
traceMultipleLines []        _  = []
traceMultipleLines (s:seeds) nn =
    let (svgElem, nn') = traceLine s nn
    in svgElem:(traceMultipleLines seeds nn')

main :: IO ()
main = putStrLn (writeSVG
                  (appendElements (plotTensorField tf cs fieldWidth fieldHeight)
                  traceLines))
