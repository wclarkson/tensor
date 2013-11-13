import Tensor
import TensorField
import Constraint
import Vector2
import SVGWriter

tf :: TensorField
tf = makeTensorField [  Linear (Vector2 2 2) 1.7 3, Linear (Vector2 8 8) 3.14 1 ]

main :: IO ()
main = putStrLn (writeSVG (plotTensorField tf 10))
