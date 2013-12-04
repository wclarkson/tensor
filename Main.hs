import Tensor
import TensorField
import Constraint
import Vector2 (Vector2 (Vector2))
import SVGWriter
import qualified NearestNeighbor as NN
import System.Random

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

traceMajorLine :: Vector2 -> SVGElem
traceMajorLine v = traceLine "green" fst v

traceMinorLine :: Vector2 -> SVGElem
traceMinorLine v = traceLine "blue" snd v

traceLine ::
  String -> ((VectorField, VectorField) -> VectorField) -> Vector2 -> SVGElem
traceLine color which v =
  let mEvs     = which (tensorfieldEigenvectors tf)
      nn       = NN.new fieldWidth fieldHeight 5
      tracePts = traceStreamline mEvs nn fieldWidth fieldHeight v 0.01 75
      vecToPair (Vector2 vx vy) = (vx, vy)
  in Polyline (map vecToPair tracePts) color 0.1

randomSeeds :: [Vector2]
randomSeeds =
  let (g,g') = split (mkStdGen 9)
      xs     = randomRs (0.0,fieldWidth) g
      ys     = randomRs (0.0,fieldHeight) g'
  in take 10 (map (uncurry Vector2) (zip xs ys))

traceLines :: [SVGElem]
traceLines = (map traceMajorLine randomSeeds)++(map traceMinorLine randomSeeds)

main :: IO ()
main = putStrLn (writeSVG
                  (appendElements
                  (plotTensorField tf cs fieldWidth fieldHeight)
                  traceLines))
