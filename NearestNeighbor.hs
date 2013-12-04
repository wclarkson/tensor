module NearestNeighbor where

import qualified Data.Vector as V
import Data.Vector ((//), (!))
import qualified Util

data Point = Point Float Float deriving Show

class NearestNeighbor a where
  data (Storage a)
  new    :: Float -> Float -> Int -> Storage a
  insert :: Storage a -> (Point, a) -> Storage a
  lookup :: Storage a -> Point -> Maybe (Point, a)

{-
instance NearestNeighbor a where
  data (Storage a) = List [(Point, a)]
  new _ _ _ = List []
  insert (List vs) v = List (v:vs)
  lookup (List []) _          = Nothing
  lookup (List vs) (Point x y) = 
    let argmin _ []     = error "argmin empty list"
        argmin _ (n:[]) = n
        argmin f (n:ns) = if f n < f s then n else s where s = argmin f ns
        dist (Point a b) = (a-x)*(a-x) + (b-y)*(b-y)
    in Just (argmin (dist . fst) vs)
-}

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
    then Buckets w h xsize ysize buckets -- do nothing if out of bounds
    else let x     = floor (px / xsize)
             y     = floor (py / ysize)
             point = (Point px py, v)
             list' = point : ((buckets ! y) ! x)
             row'   = (buckets ! y) // [(x, list')]
             cols'  = buckets // [(y, row')]
         in Buckets w h xsize ysize cols'

-- bucketsLookup has not been thoroughly tested
bucketsLookup :: Storage a -> Point -> Maybe (Point, a)
bucketsLookup (Buckets w _ xsize ysize v) (Point x y) =
  let bx = floor (x / xsize)
      by = floor (y / ysize)
      n = floor $ w / xsize
      points = [(bx+a,by+b) | a<-[-1..1], b<-[-1..1]]
      getBucket (a, b) = if not ((a >= 0) && (b >= 0) &&
                                  (a < n) && (b < n))
                         then [] else (v ! a) ! b
      blist = foldl (\lp p -> getBucket p ++ lp) [] points
      dist (Point a b) = (a-x)*(a-x) + (b-y)*(b-y)
  in case blist of [] -> Nothing
                   _  -> Just $ Util.argmin (dist . fst) blist
