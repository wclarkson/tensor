module Vector2 where

data Vector2 = Vector2 { x :: Float, y :: Float } deriving Show

add :: Vector2 -> Vector2 -> Vector2
add (Vector2 a b) (Vector2 c d) = Vector2 (a + c) (b + d)

sub :: Vector2 -> Vector2 -> Vector2
sub (Vector2 a b) (Vector2 c d) = Vector2 (a - c) (b - d)

scalarTimes :: Float -> Vector2 -> Vector2
scalarTimes c (Vector2 a b) = Vector2 (c * a) (c * b)

mag :: Vector2 -> Float
mag (Vector2 a b) = sqrt (a*a + b*b)

unit :: Vector2 -> Vector2
unit v = scalarTimes (1/(mag v)) v

sqMag :: Vector2 -> Float
sqMag (Vector2 a b) = a*a + b*b
