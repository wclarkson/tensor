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

sqMag :: Vector2 -> Float
sqMag (Vector2 a b) = a*a + b*b

isReal :: Vector2 -> Bool
isReal (Vector2 a b) = not (Prelude.isNaN a && Prelude.isNaN b) &&
                       not (isInfinite a && isInfinite b)
