module NearestNeighbor where

import qualified Data.Vector as V
import Data.Vector ((//), (!))
import Debug.Trace

data Point = Point Float Float deriving Show
{-}
data (Storage a) = Buckets { width   :: Float, height :: Float,
                             xsize   :: Float, ysize  :: Float,
                             buckets :: V.Vector (V.Vector [(Point, a)]) }
                             -}

class NearestNeighbor a where
  data (Storage a)
  new      :: Float -> Float -> Int -> Storage a
  insert   :: Storage a -> (Point, a) -> Storage a
  lookup :: Storage a -> Point -> Maybe (Point, a)


instance NearestNeighbor a where
  data (Storage a) = Buckets { width   :: Float, height :: Float,
                               xsize   :: Float, ysize  :: Float,
                               buckets :: V.Vector (V.Vector [(Point, a)]) }
                               deriving Show
  new    = bucketsNew
  insert = bucketsInsert
  lookup = bucketsLookup

bucketsNew :: Float -> Float -> Int -> Storage a
bucketsNew width height nbuckets =
  Buckets width height
          (width / fromIntegral nbuckets)
          (height / fromIntegral nbuckets)
          (V.replicate nbuckets (V.replicate nbuckets []))

bucketsInsert :: Storage a -> (Point, a) -> Storage a
bucketsInsert (Buckets w h xsize ysize buckets) (Point px py, v) =
  if not $ (px >= 0) && (py >= 0) && (px < w) && (py < h)
    then error "bounds"
    else let x     = floor (px / xsize)
             y     = floor (py / ysize)
             point = (Point px py, v)
             list' = point : ((buckets ! y) ! x)
             row'   = (buckets ! y) // [(x, list')]
             cols'  = buckets // [(y, row')]
         in Buckets w h xsize ysize cols'

-- bucketsLookup has not been thoroughly tested
bucketsLookup :: Storage a -> Point -> Maybe (Point, a)
bucketsLookup (Buckets w h xsize ysize v) (Point x y) =
  let bx = floor (x / xsize)
      by = floor (y / ysize)
      n = floor $ w / xsize
      points = [(bx+a,by+b) | a<-[-1..1], b<-[-1..1]]
      getBucket (a, b) = if not ((a >= 0)     && (b >= 0) && 
                                  (a <= n) && (b <= n))
                         then [] else (v ! a) ! b
      blist = foldl (\lp p -> getBucket p ++ lp) [] points
      dist (Point a b) = (a-x)*(a-x) + (b-y)*(b-y)
      argmin _ [] = error "argmin passed empty list"
      argmin f (n:[]) = n
      argmin f (n:ns) = if f n < f s then n else s
        where s = argmin f ns
  in case blist of [] -> Nothing
                         _  -> Just $ argmin (dist . fst) blist

b  = new 10 10 4
b' = NearestNeighbor.insert b (Point 0 0,"nutz")

main :: IO ()
main = putStrLn (show (NearestNeighbor.lookup b' (Point 0 0)))
