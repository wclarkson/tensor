{-# LANGUAGE OverloadedStrings, DeriveGeneric, NamedFieldPuns #-}

module JSONParser
where

import Data.Aeson
import GHC.Generics
import System.Environment
import qualified Data.ByteString.Lazy as B
import Constraint
import Vector2

data Input = Input
           {
            tycon :: String,
            posx  :: Float,
            posy  :: Float,
            dir   :: Float,
            mag   :: Float
            } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

contentsOfArgv1 :: IO B.ByteString
contentsOfArgv1 = do
  a <- getArgs
  if length a /= 1 then error "No input file"
                   else B.readFile (a !! 0)

inputToConstraint :: Input -> Constraint
inputToConstraint (Input "Linear" posx posy dir mag) =
    Linear (Vector2 posx posy) dir mag
inputToConstraint (Input "Radial" posx posy _ _) =
    Radial (Vector2 posx posy)
inputToConstraint _ = error "Invalid input."
