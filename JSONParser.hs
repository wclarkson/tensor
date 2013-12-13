{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module JSONParser where

import Data.Aeson
import GHC.Generics
import Constraint
import Vector2

data Input = Input {  tycon :: String,
                      posx  :: Float,
                      posy  :: Float,
                      dir   :: Float,
                      mag   :: Float  } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

inputToConstraint :: Input -> Constraint
inputToConstraint (Input "Linear" posx posy dir mag) =
    Linear (Vector2 posx posy) dir mag
inputToConstraint (Input "Radial" posx posy _ _) =
    Radial (Vector2 posx posy)
inputToConstraint _ = error "Invalid input."
