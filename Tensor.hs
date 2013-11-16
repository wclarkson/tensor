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

fromXY :: Float -> Float -> Tensor
fromXY x y = let xy = -2 * x * y
                 diffSquares = y * y - x * x
             in Tensor diffSquares xy (-1 * diffSquares)

isReal :: Tensor -> Bool
isReal (Tensor a1 b1 c1) = 
  let isreal x = not (Prelude.isNaN x || isInfinite x) 
  in isreal a1 && isreal b1 && isreal c1

add :: Tensor -> Tensor -> Tensor
add (Tensor a1 b1 d1) (Tensor a2 b2 d2) =
  Tensor (a1+a2) (b1+b2) (d1+d2)

sum :: [Tensor] -> Tensor
sum = foldl add trivial

scalarTimes :: Float -> Tensor-> Tensor
scalarTimes c (Tensor a1 b1 d1) = Tensor (c*a1) (c*b1) (c*d1)

eigenvalue :: Float -> Float -> Float -> Float
eigenvalue _ b _ =
  let ev = b -- (4*b*b + (a - d)*(a - d))
  in if isNaN ev then error "broken" else ev

eigenvectors :: Tensor -> (V2.Vector2, V2.Vector2)
eigenvectors (Tensor a1 0.0 d1) = 
  if a1 > d1 then (V2.Vector2 1 0, V2.Vector2 0 1)
             else (V2.Vector2 0 1, V2.Vector2 1 0)
eigenvectors (Tensor a1 b1 d1) =
    let eval1 = ((a1 + d1) + sqrt (4*b1*b1 + (a1 - d1)*(a1 - d1)))/2
        eval2 = ((a1 + d1) - sqrt (4*b1*b1 + (a1 - d1)*(a1 - d1)))/2
        evec1 = V2.Vector2 1 ((eval1 - a1)/b1)
        evec2 = V2.Vector2 1 ((eval2 - a1)/b1)
    --there are still unreal values somewhere
    in (evec1, evec2)
    --in (V2.removeUnreal evec1, V2.removeUnreal evec2)
  {--
  in if V2.isReal evec1 && V2.isReal evec2 then (evec1, evec2)
                                           else (V2.zero, V2.zero)
  --}
