module Vector2 where

data Vector2 = Vector2 { x :: Float, y :: Float } deriving Show

zero :: Vector2
zero = Vector2 0 0

add :: Vector2 -> Vector2 -> Vector2
add (Vector2 a b) (Vector2 c d) = Vector2 (a + c) (b + d)

sub :: Vector2 -> Vector2 -> Vector2
sub (Vector2 a b) (Vector2 c d) = Vector2 (a - c) (b - d)

scalarTimes :: Float -> Vector2 -> Vector2
scalarTimes c (Vector2 a b) = Vector2 (c * a) (c * b)

mag :: Vector2 -> Float
mag (Vector2 a b) = sqrt (a*a + b*b)

unit :: Vector2 -> Vector2
unit v = case mag v of 0 -> Vector2 0 0
                       m -> scalarTimes (1/m) v

dot :: Vector2 -> Vector2 -> Float
dot (Vector2 a1 b1) (Vector2 a2 b2) = a1*a2 + b1*b2

avg :: Vector2 -> Vector2 -> Vector2
avg v1 v2 = scalarTimes 0.5 (add v1 v2)

sqMag :: Vector2 -> Float
sqMag (Vector2 a b) = a*a + b*b

isReal :: Vector2 -> Bool
isReal (Vector2 a b) = not (Prelude.isNaN a || Prelude.isNaN b) &&
                       not (isInfinite a || isInfinite b)
