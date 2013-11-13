module Vector2 where

data Vector2 = Vector2 { x :: Float, y :: Float }

add :: Vector2 -> Vector2 -> Vector2
add (Vector2 a b) (Vector2 c d) = Vector2 (a + b) (c + d)

sub :: Vector2 -> Vector2 -> Vector2
sub (Vector2 a b) (Vector2 c d) = Vector2 (a - b) (c - d)

mag :: Vector2 -> Float
mag (Vector2 a b) = sqrt (a*a + b*b)

sqMag :: Vector2 -> Float
sqMag (Vector2 a b) = a*a + b*b
