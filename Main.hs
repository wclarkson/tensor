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

main :: IO ()
main = putStrLn (writeSVG (plotTensorField tf cs 20))
