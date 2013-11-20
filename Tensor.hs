module Tensor where

import qualified Vector2 as V2

-- A tensor is a 2x2 symmetric and traceless matrix of the form
-- R * | cos(2theta)  sin(2theta) |  = | a b |
--     | sin(2theta) -cos(2theta) |    | _ d |
-- where R >= 0 and theta is [0, 2pi)
data Tensor = Tensor { a :: Float, b :: Float, d :: Float }

trivial :: Tensor
trivial = Tensor 0 0 0

-- Creating Tensors from various types of constraints

fromRTheta :: Float -> Float -> Tensor
fromRTheta r t = Tensor (r*cos(2*t)) (r*sin(2*t)) (-1*r*cos(2*t))

fromXY :: Float -> Float -> Tensor
fromXY x y = let xy = -2 * x * y
                 diffSquares = y * y - x * x
             in Tensor diffSquares xy (-1 * diffSquares)

-- Basic Tensor operations

add :: Tensor -> Tensor -> Tensor
add (Tensor a1 b1 d1) (Tensor a2 b2 d2) =
  Tensor (a1+a2) (b1+b2) (d1+d2)

sum :: [Tensor] -> Tensor
sum = foldl add trivial

scalarTimes :: Float -> Tensor-> Tensor
scalarTimes c (Tensor a1 b1 d1) = Tensor (c*a1) (c*b1) (c*d1)

isReal :: Tensor -> Bool
isReal (Tensor a b c) = 
  let isreal x = not (Prelude.isNaN x || isInfinite x) 
  in isreal a && isreal b && isreal c

-- Calculating eigenvalues and vectors

-- returns a tuple of the (larger, smaller) eigenvectors of a tensor
eigenvalues :: Tensor -> (Float, Float)
eigenvalues (Tensor a b d) = 
  let evals f = (f (a + d) (sqrt (4*b*b + (a - d)*(a - d)))) / 2
  in  (evals (+), evals (-))

-- returns a tuple of the (major, minor) eigenvectors for a tensor
eigenvectors :: Tensor -> (V2.Vector2, V2.Vector2)
eigenvectors (Tensor a 0.0 d) = 
  if a > d then (V2.Vector2 1 0, V2.Vector2 0 1)
             else (V2.Vector2 0 1, V2.Vector2 1 0)
eigenvectors (Tensor a b d) =
    let (eval1, eval2) = eigenvalues (Tensor a b d)
        mkEvec eval    = V2.Vector2 1 ((eval - a) / b)
    in (mkEvec eval1, mkEvec eval2)


