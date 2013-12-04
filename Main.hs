import Tensor
import TensorField
import Constraint
import Vector2
import SVGWriter
import qualified NearestNeighbor as NN
import System.Random

cs :: [Constraint]
cs = [ --Radial (Vector2 10 10),
       Linear (Vector2 2 2)  0.01 3,
       Linear (Vector2 6 14) 0.6  3,
       Linear (Vector2 8 8)  1.2  3 ]

fieldWidth :: Float
fieldWidth = 20
fieldHeight :: Float
fieldHeight = 20

tf :: TensorField
tf = makeTensorField cs

traceMajorLine :: Vector2 -> SVGElem
traceMajorLine v =
  let mEvs     = fst (tensorfieldEigenvectors tf)
      nn       = NN.new 10 10 4
      tracePts = traceStreamline mEvs nn fieldWidth fieldHeight v 0.01 75
      vecToPair (Vector2 vx vy) = (vx, vy)
  in Polyline (map vecToPair tracePts) "green" 0.1

traceMinorLine :: Vector2 -> SVGElem
traceMinorLine v =
  let mEvs     = snd (tensorfieldEigenvectors tf)
      nn       = NN.new 10 10 4
      tracePts = traceStreamline mEvs nn fieldWidth fieldHeight v 0.01 75
      vecToPair (Vector2 vx vy) = (vx, vy)
  in Polyline (map vecToPair tracePts) "blue" 0.1

randomSeeds :: [Vector2]
randomSeeds = 
  let (g,g')  = split (mkStdGen 9)
      xs = randomRs (0.0,fieldWidth) g
      ys = randomRs (0.0,fieldHeight) g'
  in take 10 (map (uncurry Vector2) (zip xs ys))

traceLines :: [SVGElem]
traceLines = (map traceMajorLine randomSeeds) ++ (map traceMinorLine randomSeeds)
--traceLines = (map traceMajorLine [ Vector2 5 ty | ty<-[0,2..20] ])
--          ++ (map traceMinorLine [ Vector2 tx 5 | tx<-[0,2..20] ])

main :: IO ()
main = putStrLn (writeSVG
                  (appendElements (plotTensorField tf cs fieldWidth fieldHeight)
                  traceLines))
