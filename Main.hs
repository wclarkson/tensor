import Tensor
import TensorField
import Constraint
import Vector2
import SVGWriter

cs :: [Constraint]
--cs = [ Linear (Vector2 2 2) 0 3, Linear (Vector2 8 8) 2.7 1 ]
--cs = [ Linear (Vector2 8 8) 0.01 3 ]
cs = [ Radial (Vector2 8 8), Linear (Vector2 2 2) 0.01 3 ]

tf :: TensorField
tf = makeTensorField cs

majorEigenvectors :: VectorField
majorEigenvectors = fst (tensorfieldEigenvectors tf)

tracePts :: [Vector2]
tracePts = traceStreamline majorEigenvectors (Vector2 2 2)

vecToPair :: Vector2 -> (Float, Float)
vecToPair (Vector2 vx vy) = (vx, vy)

traceLine :: SVGElem
traceLine = Polyline (map vecToPair tracePts) "green" 0.1

main :: IO ()
main = putStrLn (writeSVG (appendElements (plotTensorField tf cs 20) [traceLine]))
