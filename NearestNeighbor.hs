module NearestNeighbor where

import qualified Data.Vector as V
import Data.Vector ((//), (!))
import qualified Vector2 as V2

--data Point = Point Float Float deriving Show

class NearestNeighbor a where
  data (Storage a)
  new    :: Float -> Float -> Int -> Storage a
  insert :: Storage a -> (V2.Vector2, a) -> Storage a
  lookup :: Storage a -> V2.Vector2 -> Maybe (V2.Vector2, a)

{-
instance NearestNeighbor a where
  data (Storage a) = List [(V2.Vector2, a)]
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
                               buckets :: V.Vector (V.Vector [(V2.Vector2, a)]) }
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

bucketsInsert :: Storage a -> (V2.Vector2, a) -> Storage a
bucketsInsert (Buckets w h xsize ysize buckets) (V2.Vector2 px py, v) =
  if not $ (px >= 0) && (py >= 0) && (px < w) && (py < h)
    then Buckets w h xsize ysize buckets -- do nothing if out of bounds
    else let x     = floor (px / xsize)
             y     = floor (py / ysize)
             point = (V2.Vector2 px py, v)
             list' = point : ((buckets ! y) ! x)
             row'   = (buckets ! y) // [(x, list')]
             cols'  = buckets // [(y, row')]
         in Buckets w h xsize ysize cols'

-- bucketsLookup has been somewhat thoroughly tested
bucketsLookup :: Storage a -> V2.Vector2 -> Maybe (V2.Vector2, a)
bucketsLookup (Buckets w _ xsize ysize v) (V2.Vector2 x y) =
  let bx = floor (x / xsize)
      by = floor (y / ysize)
      n = floor $ w / xsize
      points = [(bx+a,by+b) | a<-[-1..1], b<-[-1..1]]
      getBucket (a, b) = if not ((a >= 0) && (b >= 0) &&
                                  (a < n) && (b < n))
                         then [] else (v ! a) ! b
      blist = foldl (\lp p -> getBucket p ++ lp) [] points
      dist (V2.Vector2 a b) = (a-x)*(a-x) + (b-y)*(b-y)
      argmin _ [] = error "argmin passed empty list"
      argmin _ (n:[]) = n
      argmin f (n:ns) = if f n < f s then n else s
        where s = argmin f ns
  in case blist of [] -> Nothing
                   _  -> Just $ argmin (dist . fst) blist
