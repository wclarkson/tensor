module Constraint where

import Vector2

data Constraint = Linear { posn :: Vector2, dir :: Float, mag :: Float }
                | Radial { posn :: Vector2 }
