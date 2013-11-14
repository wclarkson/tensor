module Tensor where

import qualified Vector2 as V2

-- A tensor is a 2x2 symmetric and traceless matrix of the form
-- R * | cos(2theta)  sin(2theta) |  = | a b |
--     | sin(2theta) -cos(2theta) |    | _ d |
-- where R >= 0 and theta is [0, 2pi)
data Tensor = Tensor { a :: Float, b :: Float, d :: Float }

trivial :: Tensor
trivial = Tensor 0 0 0

fromRTheta :: Float -> Float -> Tensor
fromRTheta r t = Tensor (r*cos(2*t)) (r*sin(2*t)) (-1*r*cos(2*t))

add :: Tensor -> Tensor -> Tensor
add (Tensor a1 b1 d1) (Tensor a2 b2 d2) =
  Tensor (a1+a2) (b1+b2) (d1+d2)

sum :: [Tensor] -> Tensor
sum = foldl add trivial

scalarTimes :: Float -> Tensor-> Tensor
scalarTimes c (Tensor a1 b1 d1) = Tensor (c*a1) (c*b1) (c*d1)

eigenvectors :: Tensor -> (V2.Vector2, V2.Vector2)
eigenvectors (Tensor a1 b1 d1) =
  let eval1 = ((a1 + d1) + sqrt (4*b1*b1 + (a1 - d1)*(a1 - d1)))/2
      eval2 = ((a1 + d1) - sqrt (4*b1*b1 + (a1 - d1)*(a1 - d1)))/2
      evec1 = V2.Vector2 1 ((eval1 - a1)/b1)
      evec2 = V2.Vector2 1 ((eval2 - a1)/b1)
  in if V2.isNaN evec1 || V2.isNaN evec2 then (V2.zero, V2.zero)
                                         else (evec1, evec2)
