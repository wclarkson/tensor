import Tensor
import TensorField
import Constraint
import Vector2
import SVGWriter
import NearestNeighbor (Storage, Point (Point))
import qualified NearestNeighbor as NN

cs :: [Constraint]
cs = [ Radial (Vector2 8 8), Linear (Vector2 2 2) 0.01 3,
       Linear (Vector2 6 14) 0.6 3]

fieldWidth :: Float
fieldWidth = 20
fieldHeight :: Float
fieldHeight = 20

tf :: TensorField
tf = makeTensorField cs

-- the vector2 arg is a seed point
traceLine :: Vector2 -> Vector2 -> [Vector2] -> SVGElem
traceLine mEvs tracePts =
  let vecToPair (Vector2 vx vy) = (vx, vy)
  in Polyline (map vecToPair tracePts) "green" 0.1

traceMultipleLines :: (Num a) => [Vector2] -> Storage a -> [SVGElem]
traceMultipleLines []           nn = []
traceMultipleLines (seed:seeds) nn =
    let mEvs     = fst (tensorfieldEigenvectors tf)
        tracePts = traceStreamline mEvs nn fieldWidth fieldHeight seed 0.01 75
        nn'   = addStreamlineToNN tracePts nn
    in (traceLine mEvs tracePts seed):(traceMultipleLines seeds nn')

traceLines :: [SVGElem]
traceLines = traceMultipleLines [(Vector2 15 15), (Vector2 0 0)]
  --map traceLine [ Vector2 5 ty | ty<-[10..15] ]

main :: IO ()
main = putStrLn (writeSVG
                  (appendElements (plotTensorField tf cs fieldWidth fieldHeight)
                  traceLines))
