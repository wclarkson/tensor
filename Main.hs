import Tensor
import TensorField
import Constraint
import Vector2
import SVGWriter
import NearestNeighbor (Storage)
import qualified NearestNeighbor as NN
import System.Random
import Data.List
import Util

type Streamline = [Vector2]

data PlacementMethod = Random
                     | Furthest
                     | Improved

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

emptyNN :: Storage a
emptyNN = NN.new fieldWidth fieldHeight 4

{-}
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
  -}

streamlineFromEvs :: (Num a) => VectorField -> Storage a -> Vector2 -> Streamline
streamlineFromEvs evf nn seed =
  traceStreamline evf nn fieldWidth fieldHeight seed 0.01 75

drawStreamline :: Streamline -> SVGElem
drawStreamline vs =
  let vecToPair (Vector2 vx vy) = (vx, vy) 
  in Polyline (map vecToPair vs) "green" 0.1


randomSeeds :: Int -> [Vector2]
randomSeeds n = 
  let (g,g') = split (mkStdGen 8)
      xs     = randomRs (0.0,fieldWidth) g
      ys     = randomRs (0.0,fieldHeight) g'
  in take n (zipWith Vector2 xs ys)

sampleList xs =
  let everyNth n ys = case drop (n-1) ys of
                        (z:zs) -> z : everyNth n zs
                        []     -> []
  in everyNth 10 xs

placeSeeds :: (VectorField, VectorField) -> [Streamline] -> Int -> [Streamline]
placeSeeds _         sls 0 = sls
placeSeeds (maj,min) sls n =
  let existPts = concatMap sampleList sls
      trialPts = randomSeeds 10
      dist (Vector2 x y) (Vector2 x2 y2) = (x-x2)*(x-x2) + (y-y2)*(y-y2)
      closestPt v = case existPts of
                      [] -> 2*fieldWidth  -- this is evil
                      _  -> minimum (map (dist v) existPts)
      seed     = Util.argmax closestPt trialPts
      nn       = emptyNN
      majSl    = streamlineFromEvs maj nn seed
      minSl    = streamlineFromEvs min nn seed
  in placeSeeds (maj,min) (majSl:minSl:sls) (n - 1)

placeSeedsImproved ::
  (VectorField, VectorField) -> [Streamline] -> Int -> [Streamline]
placeSeedsImproved _         sls 0 = sls
placeSeedsImproved (maj,min) sls n =
  let existPts = map sampleList sls
      trialPts = randomSeeds 10
      nn       = emptyNN
      mkSls s  = [ streamlineFromEvs maj nn s,
                   streamlineFromEvs min nn s]
      trialSls = map mkSls trialPts
      slSets   = map (++existPts) trialSls
      --bestSls  = Util.argmin (chiSquaredEvenSpacing fieldWidth fieldHeight 4)
      --                       slSets
      bestSls = head slSets
  in placeSeedsImproved (maj,min) bestSls (n - 1)
      
-- measure the X^2 goodness of fit of a  set of streamlines to determine how
-- evenly spaced it is
chiSquaredEvenSpacing :: Float -> Float -> Float -> [[Vector2]] -> Float
chiSquaredEvenSpacing  width _ buckets vs =
  let scale (Vector2 x y) = (round $ x * buckets / width,
                             round $ y * buckets / width)
      bucketFreqs :: [Float]
      bucketFreqs = map (fromIntegral . length) $ (group . sort)
                                                $ map scale (concat vs)
      expFreq     = Data.List.sum bucketFreqs /
                      fromIntegral (length bucketFreqs)
      x2          = map (\o -> (o - expFreq)*(o - expFreq)/expFreq) bucketFreqs
  in Data.List.sum x2

placeStreamlines :: (Num a) =>
  TensorField -> Storage a -> Int -> PlacementMethod -> [[Vector2]]
placeStreamlines tf nn n Random =
  let (maj,min)   = tensorfieldEigenvectors tf
      seeds       = randomSeeds n
      traceSeed s = [ streamlineFromEvs maj nn s,
                      streamlineFromEvs min nn s ]
  in concatMap traceSeed seeds
placeStreamlines tf nn n Furthest =
  placeSeeds (tensorfieldEigenvectors tf) [] n
placeStreamlines tf nn n Improved =
  placeSeedsImproved (tensorfieldEigenvectors tf) [] n

traceLines :: [SVGElem]
traceLines = map drawStreamline $ placeStreamlines tf emptyNN 1 Improved

--traceLines = (map traceMajorLine randomSeeds) ++ (map traceMinorLine randomSeeds)

--traceLines = (map traceMajorLine [ Vector2 5 ty | ty<-[0,2..20] ])
--          ++ (map traceMinorLine [ Vector2 tx 5 | tx<-[0,2..20] ])

main :: IO ()
main = putStrLn (writeSVG
                  (appendElements (plotTensorField tf cs fieldWidth fieldHeight)
                  traceLines))
