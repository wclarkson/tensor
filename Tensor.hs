module Tensor where

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


