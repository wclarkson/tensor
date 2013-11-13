module Constraint where

data Position = Position { x :: Float, y :: Float }

data Constraint = Linear { posn :: Position, dir :: Float, mag :: Float }
                | Radial { posn :: Position, mag :: Float }
