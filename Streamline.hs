module Streamline (PlacementMethod, traceLines) where

import TensorField
import Vector2 (Vector2 (Vector2))
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

streamlineFromEvs :: (Num a) =>
  VectorField -> Storage a -> Vector2 -> Float -> Float -> Streamline
streamlineFromEvs evf nn seed fw fh =
  traceStreamline evf nn fw fh seed 0.01 75

drawStreamline :: Streamline -> SVGElem
drawStreamline vs =
  let vecToPair (Vector2 vx vy) = (vx, vy)
  in Polyline (map vecToPair vs) "green" 0.1

randomSeeds :: Int -> Float -> Float -> [Vector2]
randomSeeds n fw fh =
  let (g,g') = split (mkStdGen 8)
      xs     = randomRs (0.0,fw) g
      ys     = randomRs (0.0,fh) g'
  in take n (zipWith Vector2 xs ys)

sampleList :: [a] -> [a]
sampleList xs =
  let everyNth n ys = case drop (n-1) ys of
                        (z:zs) -> z : everyNth n zs
                        []     -> []
  in everyNth 10 xs

placeSeeds :: (VectorField, VectorField) -> [Streamline] -> Int ->
  Float -> Float -> [Streamline]
placeSeeds _         sls 0 _  _  = sls
placeSeeds (maj,min) sls n fw fh =
  let existPts = concatMap sampleList sls
      trialPts = randomSeeds 10 fw fh
      dist (Vector2 x y) (Vector2 x2 y2) = (x-x2)*(x-x2) + (y-y2)*(y-y2)
      closestPt v = case existPts of
                      [] -> 2*fw  -- this is evil
                      _  -> minimum (map (dist v) existPts)
      seed     = Util.argmax closestPt trialPts
      nn       = NN.new fw fh 4
      majSl    = streamlineFromEvs maj nn seed fw fh
      minSl    = streamlineFromEvs min nn seed fw fh
  in placeSeeds (maj,min) (majSl:minSl:sls) (n - 1) fw fh

placeSeedsImproved ::
  (VectorField, VectorField) -> [Streamline] -> Int -> Float -> Float -> [Streamline]
placeSeedsImproved _         sls 0 _  _  = sls
placeSeedsImproved (maj,min) sls n fw fh =
  let existPts = map sampleList sls
      trialPts = randomSeeds 10 fw fh
      nn       = NN.new fw fh 4
      mkSls s  = [ streamlineFromEvs maj nn s fw fh,
                   streamlineFromEvs min nn s fw fh ]
      trialSls = map mkSls trialPts
      slSets   = map (++existPts) trialSls
      bestSls  = Util.argmin (chiSquaredEvenSpacing fw fh 4) slSets
  in placeSeedsImproved (maj,min) bestSls (n - 1) fw fh

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
  TensorField -> Storage a -> Int -> Float -> Float -> PlacementMethod -> [[Vector2]]
placeStreamlines tf nn n fw fh Random =
  let (maj,min)   = tensorfieldEigenvectors tf
      seeds       = randomSeeds n fw fh
      traceSeed s = [ streamlineFromEvs maj nn s fw fh,
                      streamlineFromEvs min nn s fw fh ]
  in concatMap traceSeed seeds
placeStreamlines tf nn n fw fh Furthest =
  placeSeeds (tensorfieldEigenvectors tf) [] n fw fh
placeStreamlines tf nn n fw fh Improved =
  placeSeedsImproved (tensorfieldEigenvectors tf) [] n fw fh

traceLines :: TensorField -> Float -> Float -> [SVGElem]
traceLines tf fw fh =
  map drawStreamline $ placeStreamlines tf (NN.new fw fh 4) 8 fw fh Furthest
