import Tensor
import TensorField
import Constraint
import Vector2
import SVGWriter
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

traceLine :: Vector2 -> SVGElem
traceLine v =
  let mEvs     = fst (tensorfieldEigenvectors tf)
      nn       = NN.new 10 10 4
      tracePts = traceStreamline mEvs nn fieldWidth fieldHeight v 0.01 75
      vecToPair (Vector2 vx vy) = (vx, vy)
  in Polyline (map vecToPair tracePts) "green" 0.1

traceLines :: [SVGElem]
traceLines = [traceLine (Vector2 15 15)]
  --map traceLine [ Vector2 5 ty | ty<-[10..15] ]

main :: IO ()
main = putStrLn (writeSVG
                  (appendElements (plotTensorField tf cs fieldWidth fieldHeight)
                  traceLines))
